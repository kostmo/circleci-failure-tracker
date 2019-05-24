{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                     (guard, unless, when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (ExceptT (ExceptT), except,
                                                    runExceptT)
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Char8        as LBSC
import           Data.Default                      (def)
import           Data.List                         (filter)
import           Data.List.Split                   (splitOn)
import qualified Data.Maybe                        as Maybe
import qualified Data.Set                          as Set
import           Data.String                       (fromString)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT
import qualified Data.Time.Clock                   as Clock
import qualified Data.Vault.Lazy                   as Vault
import qualified GitHub.Data.Webhooks.Validate     as GHValidate
import qualified Network.URI                       as URI
import           Network.Wai
import           Network.Wai.Middleware.ForceSSL   (forceSSL)
import           Network.Wai.Middleware.Static
import           Network.Wai.Session               (SessionStore)
import           Network.Wai.Session               (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           Options.Applicative
import qualified Safe
import           System.Environment                (lookupEnv)
import           System.FilePath
import           Text.Read                         (readMaybe)
import           Web.ClientSession                 (getDefaultKey)
import qualified Web.Scotty                        as S
import qualified Web.Scotty.Internal.Types         as ScottyTypes

import qualified ApiPost
import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Breakages
import qualified Builds
import qualified DbHelpers
import qualified DbInsertion
import qualified GitRev
import qualified IDP
import qualified Scanning
import qualified ScanPatterns
import qualified Session
import qualified SqlRead
import qualified SqlWrite
import qualified StatusEvent
import qualified Types
import qualified WebApi
import qualified Webhooks


myAppStatusContext :: Text
myAppStatusContext = "flaky-checker"


pattern_from_parms :: ScottyTypes.ActionT LT.Text IO ScanPatterns.Pattern
pattern_from_parms = do

  expression <- S.param "pattern"
  is_regex_str <- S.param "is_regex"
  is_nondeterministic_str <- S.param "is_nondeterministic"
  description <- S.param "description"
  tags <- S.param "tags"
  applicable_steps <- S.param "applicable_steps"

  let is_regex = is_regex_str == ("true" :: Text)
      is_nondeterministic = is_nondeterministic_str == ("true" :: Text)
      match_expression = if is_regex
        then ScanPatterns.RegularExpression expression is_nondeterministic
        else ScanPatterns.LiteralExpression expression

  return $ ScanPatterns.NewPattern
    match_expression
    description
    (listify tags)
    (listify applicable_steps)
    1
    False
  where
    -- TODO use semicolon as delimiter to be consistent with the database
    listify = filter (not . T.null) . map (T.strip . T.pack) . splitOn ","


data SetupData = SetupData {
    _setup_static_base     :: String
  , _setup_github_config   :: AuthConfig.GithubConfig
  , _setup_connection_data :: DbHelpers.DbConnectionData
  }


data PersistenceData = PersistenceData {
    _setup_cache   :: Types.CacheStore
  , _setup_session :: Vault.Key (Session IO String String)
  , _setup_store   :: SessionStore IO String String
  }


breakage_report_from_parms :: ScottyTypes.ActionT LT.Text IO (AuthStages.Username -> Breakages.BreakageReport)
breakage_report_from_parms = do

  notes <- S.param "notes"
  is_broken <- S.param "is_broken"
  implicated_revision <- S.param "implicated_revision"
  revision <- S.param "revision"

  -- TODO - structural validation on both Git hashes
  return $ Breakages.NewBreakageReport
    revision
    (if T.null implicated_revision then Nothing else Just implicated_revision)
    is_broken
    notes


get_circleci_failure :: Text -> Clock.UTCTime -> StatusEvent.GitHubStatusEventSetter -> Maybe Builds.Build
get_circleci_failure sha1 current_time event_setter = do
  guard $ circleci_context_prefix `T.isPrefixOf` context_text
  parsed_uri <- URI.parseURI $ LT.unpack url_text
  last_segment <- Safe.lastMay $ splitOn "/" $ URI.uriPath parsed_uri
  build_number <- readMaybe last_segment
  return $ Builds.NewBuild (Builds.NewBuildNumber build_number) sha1 current_time build_name ""

  where
    context_text = LT.toStrict context
    circleci_context_prefix = "ci/circleci: "
    build_name = T.drop (T.length circleci_context_prefix) context_text

    context = StatusEvent._context event_setter
    url_text = StatusEvent._target_url event_setter


handleStatusWebhook ::
     DbHelpers.DbConnectionData
  -> Text -- ^ access token
  -> Webhooks.GitHubStatusEvent
  -> IO (Either LT.Text ())
handleStatusWebhook db_connection_data access_token status_event = do
  current_time <- Clock.getCurrentTime
  putStrLn $ "Notified of state: " ++ notified_status_state_string ++ " at " ++ show current_time

  -- Do not act on receipt of statuses from the context I have created, or else
  -- we may get stuck in an infinite notification loop
  runExceptT $ when (is_not_my_own_context && is_failure_notification) $ do

    liftIO $ putStrLn $ "Notified status context was: " ++ notified_status_context_string

    let notified_status_url_string = LT.unpack (Webhooks.target_url status_event)
    when ("ci/circleci" `T.isPrefixOf` notified_status_context_text) $ do

      liftIO $ putStrLn $ "CircleCI URL was: " ++ notified_status_url_string


    let owner_repo_text = Webhooks.name status_event
        splitted = splitOn "/" $ LT.unpack owner_repo_text

    owned_repo <- except $ case splitted of
      (org:repo:[]) -> Right $ DbHelpers.OwnerAndRepo org repo
      _ -> Left $ "un-parseable owner/repo text: " <> owner_repo_text

    failed_statuses_list <- ExceptT $ Auth.getFailedStatuses access_token owned_repo sha1

    let circleci_failed_builds = Maybe.mapMaybe (get_circleci_failure (LT.toStrict $ Webhooks.sha status_event) current_time) failed_statuses_list
        scannable_build_numbers = map Builds.build_id circleci_failed_builds

    liftIO $ do
      conn <- DbHelpers.get_connection db_connection_data
      scan_resources <- Scanning.prepare_scan_resources conn
      SqlWrite.store_builds_list conn circleci_failed_builds
      Scanning.scan_builds scan_resources $ Left $ Set.fromList scannable_build_numbers


    let total_failcount = length failed_statuses_list
        flaky_count = 0 -- FIXME

    liftIO $ putStrLn $ "KARL: There are " <> show total_failcount <> " failed builds"

    -- Filter out the CircleCI builds

    -- Check if the builds have been scanned yet.
    -- Scan each CircleCI build that needs to be scanned.

    -- For each match, check if that match's pattern is tagged as "flaky".


    -- TODO RE-enable this
    {-
    post_result <- ExceptT $ ApiPost.postCommitStatus
      access_token
      owned_repo
      sha1
      (gen_flakiness_status status_event flaky_count total_failcount)

    liftIO $ SqlWrite.insert_posted_github_status db_connection_data sha1 owned_repo post_result
    -}
    return ()

  where
    notified_status_state_string = LT.unpack (Webhooks.state status_event)
    is_failure_notification = notified_status_state_string == "failure"

    notified_status_context_string = LT.unpack $ Webhooks.context status_event
    notified_status_context_text = LT.toStrict $ Webhooks.context status_event
    is_not_my_own_context = notified_status_context_text /= myAppStatusContext
    sha1 = LT.toStrict $ Webhooks.sha status_event


gen_flakiness_status :: Webhooks.GitHubStatusEvent -> Int -> Int -> StatusEvent.GitHubStatusEventSetter
gen_flakiness_status status_event flaky_count total_failcount = StatusEvent.GitHubStatusEventSetter
  description
  status_string
  ("https://circle.pytorch.org/commit-details.html?sha1=" <> Webhooks.sha status_event)
  (LT.fromStrict myAppStatusContext)
  where
    description = LT.pack $ show flaky_count <> "/" <> show total_failcount <> " flaky, " <> "/" <> " KPs"
    status_string = if flaky_count == total_failcount
      then "success"
      else "failure"


github_event_endpoint :: DbHelpers.DbConnectionData -> AuthConfig.GithubConfig -> S.ScottyM ()
github_event_endpoint connection_data github_config  = do
  S.post "/api/github-event" $ do

--    liftIO $ putStrLn "GOT A POST..."

    maybe_signature_header <- S.header "X-Hub-Signature"
    rq_body <- S.body

    let is_signature_valid = GHValidate.isValidPayload
          (AuthConfig.webhook_secret github_config)
          (LT.toStrict <$> maybe_signature_header)
          (LBS.toStrict rq_body)

    maybe_event_type <- S.header "X-GitHub-Event"
    case maybe_event_type of
      Nothing -> return ()
      Just event_type -> if not is_signature_valid
        then return ()
        else if event_type /= "status"
          then do
            S.json =<< return ["hello" :: String]
          else do
            body_json <- S.jsonData
            liftIO $ handleStatusWebhook connection_data (AuthConfig.personal_access_token github_config) body_json
            S.json =<< return ["hello" :: String]


echo_endpoint :: S.ScottyM ()
echo_endpoint = S.post "/api/echo" $ do
  body <- S.body
  headers <- S.headers

  liftIO $ do
    putStrLn "===== HEADERS ===="
    mapM_ (\(x, y) -> putStrLn $ LT.unpack $ x <> ": " <> y) headers
    putStrLn "== END HEADERS ==="
    putStrLn "====== BODY ======"
    putStrLn $ LBSC.unpack body
    putStrLn "==== END BODY ===="


scottyApp :: PersistenceData -> SetupData -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp (PersistenceData cache session store) (SetupData static_base github_config connection_data) = do

    S.middleware $ withSession store (fromString "SESSION") def session

    S.middleware $ staticPolicy (noDots >-> addBase static_base)

    unless (AuthConfig.is_local github_config) $
      S.middleware $ forceSSL


    -- For debugging only
    when (AuthConfig.is_local github_config) echo_endpoint

    github_event_endpoint connection_data github_config

    S.post "/api/report-breakage" $ do

      breakage_report_partial <- breakage_report_from_parms
      let callback_func user_alias = SqlWrite.api_new_breakage_report connection_data breakage_report
            where
              breakage_report = breakage_report_partial user_alias

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/fetch-master-commits" $ do
      S.json $ ("TODO" :: String)


    S.post "/api/new-pattern-insert" $ do

      new_pattern <- pattern_from_parms
      let callback_func user_alias = SqlWrite.api_new_pattern connection_data user_alias new_pattern

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ DbInsertion.toInsertionResponse github_config insertion_result


    -- XXX IMPORTANT:
    -- The session cookie is specific to the parent dir of the path.
    -- So with the path "/api/callback", only HTTP accesses to paths
    -- at or below the "/api/" path will be members of the same session.
    -- Consequentially, a cookie set (namely, the github access token)
    -- in a request to a certain path will only be accessible to
    -- other requests at or below that same parent directory.
    S.get "/api/github-auth-callback" $ do
      rq <- S.request
      let Just (_sessionLookup, sessionInsert) = Vault.lookup session (vault rq)
      Auth.callbackH cache github_config (sessionInsert Auth.githubAuthTokenSessionKey)

    S.get "/logout" $ Auth.logoutH cache

    S.get "/api/failed-commits-by-day" $
      S.json =<< liftIO (SqlRead.api_failed_commits_by_day connection_data)

    {-
    S.get "/api/is-ancestor" $ do
      ancestor_sha1_text <- S.param "ancestor"
      ancestor_sha1_text <- S.param "descendent"

      S.json =<< liftIO (SqlRead.api_jobs connection_data)
    -}

    S.get "/api/job" $
      S.json =<< liftIO (SqlRead.api_jobs connection_data)

    S.get "/api/log-size-histogram" $
      S.json =<< liftIO (SqlRead.api_line_count_histogram connection_data)

    S.get "/api/step" $
      S.json =<< liftIO (SqlRead.api_step connection_data)

    S.get "/api/commit-info" $ do
      commit_sha1_text <- S.param "sha1"
      let either_validated_sha1 = GitRev.validateSha1 commit_sha1_text
      json_result <- case either_validated_sha1 of
        Left err_text -> return $ WebApi.toJsonEither $ Left err_text
        Right sha1 -> liftIO $ do
          result <- SqlRead.count_revision_builds connection_data sha1
          return $ WebApi.toJsonEither $ Right result

      S.json json_result

    S.get "/api/commit-builds" $ do
      commit_sha1_text <- S.param "sha1"
      let either_validated_sha1 = GitRev.validateSha1 commit_sha1_text
      json_result <- case either_validated_sha1 of
        Left err_text -> return $ WebApi.toJsonEither $ Left err_text
        Right sha1 -> liftIO $ do
          result <- SqlRead.get_revision_builds connection_data sha1
          return $ WebApi.toJsonEither $ Right result

      S.json json_result

    S.get "/api/new-pattern-test" $ do
      buildnum_str <- S.param "build_num"
      new_pattern <- pattern_from_parms
      S.json =<< (liftIO $ SqlWrite.api_new_pattern_test connection_data (Builds.NewBuildNumber $ read buildnum_str) new_pattern)

    S.get "/api/tag-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_tags connection_data term)

    S.get "/api/step-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_steps connection_data term)

    S.get "/api/branch-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_branches connection_data term)

    S.get "/api/random-scannable-build" $
      S.json =<< liftIO (SqlRead.api_random_scannable_build connection_data)

    S.get "/api/summary" $
      S.json =<< liftIO (SqlRead.api_summary_stats connection_data)

    S.get "/api/unmatched-builds" $
      S.json =<< liftIO (SqlRead.api_unmatched_builds connection_data)

    S.get "/api/idiopathic-failed-builds" $
      S.json =<< liftIO (SqlRead.api_idiopathic_builds connection_data)

    S.get "/api/disk" $ do
      S.json =<< liftIO WebApi.api_disk_space

    S.get "/api/pattern" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.api_single_pattern connection_data $ read pattern_id)

    S.get "/api/patterns-dump" $ do
      S.json =<< liftIO (SqlRead.dump_patterns connection_data)

    S.post "/api/patterns-restore" $ do
      body_json <- S.jsonData

      let callback_func user_alias = SqlWrite.restore_patterns connection_data user_alias body_json

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.get "/api/patterns" $ do
      S.json =<< liftIO (SqlRead.api_patterns connection_data)

    S.get "/api/patterns-branch-filtered" $ do
      branches <- S.param "branches"
      liftIO $ putStrLn $ "Got branch list: " ++ show branches
      S.json =<< liftIO (SqlRead.api_patterns_branch_filtered connection_data branches)

    S.get "/api/single-build-info" $ do
      build_id <- S.param "build_id"
      S.json =<< (liftIO $ SqlRead.get_build_info connection_data build_id)

    S.get "/api/build-pattern-matches" $ do
      build_id <- S.param "build_id"
      S.json =<< (liftIO $ SqlRead.get_build_pattern_matches connection_data build_id)

    S.get "/api/best-pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.get_best_pattern_matches connection_data pattern_id)

    S.get "/api/pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.get_pattern_matches connection_data pattern_id)

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file $ static_base </> "images/favicon.ico"

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file $ static_base </> "index.html"


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe (serverPort args) $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  cache <- Session.initCacheStore
  IDP.initIdps cache github_config

  session <- Vault.newKey
  store <- fmap clientsessionStore getDefaultKey

  S.scotty prt $ scottyApp (PersistenceData cache session store) credentials_data

  where

    credentials_data = SetupData static_base github_config connection_data
    static_base = staticBase args

    github_config = AuthConfig.GithubConfig
      (runningLocally args)
      (gitHubClientID args)
      (gitHubClientSecret args)
      (gitHubPersonalAccessToken args)
      (gitHubWebhookSecret args)

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = "logan"
      , DbHelpers.dbPassword = dbPassword args
      }


data CommandLineArgs = NewCommandLineArgs {
    serverPort                :: Int
  , staticBase                :: String
  , dbHostname                :: String
  , dbPassword                :: String
  , gitHubClientID            :: Text
  , gitHubClientSecret        :: Text
  , gitHubPersonalAccessToken :: Text
  , gitHubWebhookSecret       :: Text
  , runningLocally            :: Bool
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "port"       <> value 3001           <> metavar "PORT"
    <> help "Webserver port")
  <*> strOption   (long "data-path" <> value "/data/static" <> metavar "STATIC_DATA"
    <> help "Path to static data files")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> value "logan01" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
   -- Note: this is not the production password; this default is only for local testing
  <*> strOption   (long "github-client-id" <> metavar "GITHUB_CLIENT_ID"
    <> help "Client ID for GitHub app")
  <*> strOption   (long "github-client-secret" <> metavar "GITHUB_CLIENT_SECRET"
    <> help "Client secret for GitHub app")
  <*> strOption   (long "github-personal-access-token" <> metavar "GITHUB_PERSONAL_ACCESS_TOKEN"
    <> help "For debugging purposes. This will be removed eventually")
  <*> strOption   (long "github-webhook-secret" <> metavar "GITHUB_WEBHOOK_SECRET"
    <> help "GitHub webhook secret")
  <*> switch      (long "local"
    <> help "Webserver is being run locally, so don't redirect HTTP to HTTPS")


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "CircleCI failure log analsys webserver"
     <> header "webapp - user frontend" )
