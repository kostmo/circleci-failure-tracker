{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                     (unless, when)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.ByteString.Lazy.Char8        as LBSC
import           Data.Default                      (def)
import           Data.Either.Utils                 (maybeToEither)
import           Data.List                         (filter)
import           Data.List.Split                   (splitOn)
import qualified Data.Maybe                        as Maybe
import           Data.String                       (fromString)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT
import qualified Data.Vault.Lazy                   as Vault
import           Network.Wai
import           Network.Wai.Middleware.ForceSSL   (forceSSL)
import           Network.Wai.Middleware.Static
import           Network.Wai.Session               (SessionStore)
import           Network.Wai.Session               (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           Options.Applicative
import           System.Environment                (lookupEnv)
import           System.FilePath
import           Text.Read                         (readMaybe)
import           Web.ClientSession                 (getDefaultKey)
import qualified Web.Scotty                        as S
import qualified Web.Scotty.Internal.Types         as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Breakages
import qualified Builds
import qualified DbHelpers
import qualified DbInsertion
import qualified GitRev
import qualified ScanPatterns
import qualified Session
import qualified SqlRead
import qualified SqlWrite
import qualified StatusUpdate
import qualified Types
import qualified WebApi


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
  step_id <- S.param "step_id"

  let maybe_implicated_revision = if T.null implicated_revision then Nothing else Just implicated_revision

  -- TODO - structural validation on both Git hashes
  return $ Breakages.NewBreakageReport
    (Builds.NewBuildStepId step_id)
    maybe_implicated_revision
    is_broken
    notes


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


    S.post "/api/github-event" $ StatusUpdate.github_event_endpoint connection_data github_config

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
      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            SqlWrite.api_new_pattern conn $ Left (new_pattern, user_alias)

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
      let Just (_sessionLookup, sessionInsert) = Vault.lookup session $ vault rq
      Auth.callbackH cache github_config $ sessionInsert Auth.githubAuthTokenSessionKey

    S.get "/logout" $ Auth.logoutH cache

    S.get "/api/status-postings-by-day" $
      S.json =<< liftIO (SqlRead.api_status_postings_by_day connection_data)

    S.get "/api/failed-commits-by-day" $
      S.json =<< liftIO (SqlRead.api_failed_commits_by_day connection_data)

    {-
    S.get "/api/is-ancestor" $ do
      ancestor_sha1_text <- S.param "ancestor"
      ancestor_sha1_text <- S.param "descendent"

      S.json =<< liftIO (SqlRead.api_jobs connection_data)
    -}

    S.get "/api/test-failures" $
      S.json =<< liftIO (do
        either_items <- SqlRead.api_test_failures connection_data
        return $ WebApi.toJsonEither either_items)

    S.get "/api/posted-statuses" $
      S.json =<< liftIO (SqlRead.api_posted_statuses connection_data)

    S.get "/api/job" $
      S.json =<< liftIO (SqlRead.api_jobs connection_data)

    S.get "/api/log-storage-stats" $
      S.json =<< liftIO (SqlRead.api_storage_stats connection_data)

    S.get "/api/log-size-histogram" $
      S.json =<< liftIO (SqlRead.api_byte_count_histogram connection_data)

    S.get "/api/log-lines-histogram" $
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
      liftIO $ putStrLn $ "Testing pattern..."
      buildnum <- S.param "build_num"
      new_pattern <- pattern_from_parms
      S.json =<< liftIO (do
        foo <- SqlWrite.api_new_pattern_test connection_data (Builds.NewBuildNumber buildnum) new_pattern
        return $ WebApi.toJsonEither foo)

    S.get "/api/tag-suggest" $ do
      term <- S.param "term"
      S.json =<< liftIO (SqlRead.api_autocomplete_tags connection_data term)

    S.get "/api/step-suggest" $ do
      term <- S.param "term"
      S.json =<< liftIO (SqlRead.api_autocomplete_steps connection_data term)

    S.get "/api/branch-suggest" $ do
      term <- S.param "term"
      S.json =<< liftIO (SqlRead.api_autocomplete_branches connection_data term)

    S.get "/api/random-scannable-build" $
      S.json =<< liftIO (SqlRead.api_random_scannable_build connection_data)

    S.get "/api/summary" $
      S.json =<< liftIO (SqlRead.api_summary_stats connection_data)

    S.get "/api/unmatched-builds" $
      S.json =<< liftIO (SqlRead.api_unmatched_builds connection_data)

    S.get "/api/unmatched-builds-for-commit" $ do
      commit_sha1_text <- S.param "sha1"
      S.json =<< liftIO (SqlRead.api_unmatched_commit_builds connection_data commit_sha1_text)

    S.get "/api/idiopathic-failed-builds" $
      S.json =<< liftIO (SqlRead.api_idiopathic_builds connection_data)

    S.get "/api/idiopathic-failed-builds-for-commit" $ do
      commit_sha1_text <- S.param "sha1"
      S.json =<< liftIO (SqlRead.api_idiopathic_commit_builds connection_data commit_sha1_text)


    -- | Access-controlled endpoint
    S.get "/api/view-log" $ do
      build_id <- S.param "build_id"

      let callback_func :: AuthStages.Username -> IO (Either Text Text)
          callback_func _user_alias = do
            conn <- DbHelpers.get_connection connection_data
            maybe_log <- SqlRead.read_log conn $ Builds.NewBuildNumber build_id
            return $ maybeToEither "log not in database" maybe_log

      rq <- S.request
      either_log_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither either_log_result

    S.get "/api/pattern" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< liftIO (SqlRead.api_single_pattern connection_data pattern_id)

    S.get "/api/patterns-dump" $ do
      S.json =<< liftIO (SqlRead.dump_patterns connection_data)

    S.get "/api/presumed-stable-branches-dump" $ do
      S.json =<< liftIO (SqlRead.dump_presumed_stable_branches connection_data)

    S.post "/api/patterns-restore" $ do
      body_json <- S.jsonData

      let callback_func _user_alias = SqlWrite.restore_patterns connection_data body_json

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.get "/api/commit-breakage-reports" $ do
      commit_sha1_text <- S.param "sha1"
      S.json =<< liftIO (SqlRead.api_commit_breakage_reports connection_data commit_sha1_text)

    S.get "/api/patterns" $ do
      S.json =<< liftIO (SqlRead.api_patterns connection_data)

    S.get "/api/patterns-branch-filtered" $ do
      branches <- S.param "branches"
      liftIO $ putStrLn $ "Got branch list: " ++ show branches
      S.json =<< liftIO (SqlRead.api_patterns_branch_filtered connection_data branches)

    S.get "/api/single-build-info" $ do
      build_id <- S.param "build_id"
      S.json =<< liftIO (SqlRead.get_build_info connection_data build_id)

    S.get "/api/best-build-match" $ do
      build_id <- S.param "build_id"
      S.json =<< liftIO (SqlRead.get_best_build_match connection_data build_id)

    S.get "/api/build-pattern-matches" $ do
      build_id <- S.param "build_id"
      S.json =<< liftIO (SqlRead.get_build_pattern_matches connection_data build_id)

    S.get "/api/best-pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< liftIO (SqlRead.get_best_pattern_matches connection_data pattern_id)

    S.get "/api/pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< liftIO (SqlRead.get_pattern_matches connection_data pattern_id)

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

  -- TODO get rid of this
  cache <- Session.initCacheStore
  AuthConfig.initIdps cache github_config

  session <- Vault.newKey
  store <- fmap clientsessionStore getDefaultKey

  let persistence_data = PersistenceData cache session store

  {-
  when (AuthConfig.is_local github_config) $ do
    -- XXX FOR TESTING ONLY
    putStrLn "Starting test..."

    let computation = do
          runExceptT $ handleFailedStatuses
            connection_data
            (AuthConfig.personal_access_token github_config)
            (DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name)
           "39cedc8474aa641b30b427de8f90014ba3d20c13"
          putStrLn "Ending test..."
          return ()

    forkIO computation
    putStrLn "Forking from test..."
  -}


  S.scotty prt $ scottyApp persistence_data credentials_data

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
