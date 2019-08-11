{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT (ExceptT), except,
                                                  runExceptT)
import           Control.Monad.Trans.Reader      (ReaderT, ask, runReaderT)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Bifunctor                  (first)
import qualified Data.ByteString.Lazy.Char8      as LBSC
import           Data.Default                    (def)
import           Data.Either.Utils               (maybeToEither)
import           Data.Functor                    (($>))
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Vault.Lazy                 as Vault
import           Database.PostgreSQL.Simple      (Connection)
import           Network.Wai
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.Static   hiding ((<|>))
import           Network.Wai.Session             (Session, SessionStore,
                                                  withSession)
import           System.FilePath
import qualified Web.Scotty                      as S
import qualified Web.Scotty.Internal.Types       as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbInsertion
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Pagination
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusUpdate
import qualified Types
import qualified WebApi
import qualified WeeklyStats


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


data MutablePatternParms = MutablePatternParms {
    pat_is_nondeterminisitc :: Bool
  , pat_description         :: Text
  , pat_tags_raw_text       :: String
  , pat_specificity         :: Int
  }


checkboxIsTrue :: Text -> Bool
checkboxIsTrue = (== ("true" :: Text))


getMutablePatternParms :: ScottyTypes.ActionT LT.Text IO MutablePatternParms
getMutablePatternParms = do
  is_nondeterministic <- checkboxIsTrue <$> S.param "is_nondeterministic"

  MutablePatternParms is_nondeterministic
    <$> S.param "description"
    <*> S.param "tags"
    <*> S.param "specificity"


patternFromParms :: ScottyTypes.ActionT LT.Text IO ScanPatterns.Pattern
patternFromParms = do

  mutable_pattern_parms <- getMutablePatternParms

  expression <- S.param "pattern"
  is_regex <- checkboxIsTrue <$> S.param "is_regex"

  use_lines_from_end <- checkboxIsTrue <$> S.param "use_lines_from_end"
  applicable_steps <- S.param "applicable_steps"

  let match_expression = ScanPatterns.toMatchExpression is_regex expression $ pat_is_nondeterminisitc mutable_pattern_parms

  lines_from_end <- if use_lines_from_end
    then Just <$> S.param "lines_from_end"
    else return Nothing

  return $ ScanPatterns.NewPattern
    match_expression
    (pat_description mutable_pattern_parms)
    (DbHelpers.cleanSemicolonDelimitedList $ pat_tags_raw_text mutable_pattern_parms)
    (DbHelpers.cleanSemicolonDelimitedList applicable_steps)
    (pat_specificity mutable_pattern_parms)
    False
    lines_from_end


validateMaybeRevision ::
     LT.Text
  -> ScottyTypes.ActionT LT.Text IO (Either Text (Maybe GitRev.GitSha1))
validateMaybeRevision key = do
  implicated_revision <- S.param key
  return $ if T.null implicated_revision
    then Right Nothing
    else do
      validated_revision <- GitRev.validateSha1 implicated_revision
      return $ Just validated_revision


echoEndpoint :: S.ScottyM ()
echoEndpoint = S.post "/api/echo" $ do
  body <- S.body
  headers <- S.headers

  liftIO $ do
    putStrLn "===== HEADERS ===="
    mapM_ (\(x, y) -> putStrLn $ LT.unpack $ x <> ": " <> y) headers
    putStrLn "== END HEADERS ==="
    putStrLn "====== BODY ======"
    putStrLn $ LBSC.unpack body
    putStrLn "==== END BODY ===="


getOffsetMode :: ScottyTypes.ActionT LT.Text IO Pagination.ParentOffsetMode
getOffsetMode = do
  offset_count <- S.param "offset"
  starting_commit <- S.param "sha1"
  use_sha1_offset <- S.param "use_sha1_offset"
  use_commit_index_bounds <- S.param "use_commit_index_bounds"
  min_commit_index <- S.param "min_commit_index"
  max_commit_index <- S.param "max_commit_index"
  commit_count <- S.param "count"

  let
    offset_mode
      | checkboxIsTrue use_sha1_offset = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Commit $ Builds.RawCommit starting_commit)
            commit_count
      | checkboxIsTrue use_commit_index_bounds = Pagination.CommitIndices $
          WeeklyStats.InclusiveNumericBounds
            min_commit_index
            max_commit_index
      | otherwise = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Count offset_count)
            commit_count

  return offset_mode


jsonDbGet :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> ScottyTypes.RoutePattern
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO a)
  -> S.ScottyM ()
jsonDbGet connection_data endpoint_path f =

  S.get endpoint_path $ S.json =<< run_with_connection =<< f
  where
    wrapped_connection = liftIO $ DbHelpers.get_connection connection_data
    run_with_connection = liftIO . (=<< wrapped_connection) . runReaderT


jsonAuthorizedDbInteract :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
jsonAuthorizedDbInteract connection_data session github_config f = do

  func <- f

  let callback_func _user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func conn

  rq <- S.request
  insertion_result <- liftIO $
    Auth.getAuthenticatedUser rq session github_config callback_func

  S.json $ WebApi.toJsonEither insertion_result


breakageCauseReport ::
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO ()
breakageCauseReport connection_data session github_config = do
    breakage_sha1 <- S.param "cause_sha1"
    notes <- S.param "notes"
    jobs_delimited <- S.param "jobs"

    is_ongoing_text <- S.param "is_ongoing"
    last_affected_sha1 <- S.param "last_affected_sha1"

    failure_mode_id <- S.param "failure_mode_id"

    let is_still_ongoing = checkboxIsTrue is_ongoing_text

    rq <- S.request
    insertion_result <- liftIO $ runExceptT $ do

      let callback_func user_alias = runExceptT $ do

            conn <- liftIO $ DbHelpers.get_connection connection_data
            SqlWrite.reportBreakage
              (SqlRead.AuthConnection conn user_alias)
              jobs_delimited
              failure_mode_id
              is_still_ongoing
              (Builds.RawCommit last_affected_sha1)
              (Builds.RawCommit breakage_sha1)
              notes

      ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
    S.json $ WebApi.toJsonEither insertion_result


breakageResolutionReport ::
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO ()
breakageResolutionReport connection_data session github_config = do

    sha1 <- S.param "sha1"
    cause_ids_delimited <- S.param "causes"

    rq <- S.request
    insertion_result <- liftIO $ runExceptT $ do

      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            runReaderT
              (SqlWrite.apiCodeBreakageResolutionInsertMultiple sha1 cause_ids_delimited)
              (SqlRead.AuthConnection conn user_alias)


      ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
    S.json $ WebApi.toJsonEither insertion_result


postWithAdminToken :: (FromJSON t, ToJSON b) =>
     DbHelpers.DbConnectionData
  -> AuthConfig.GithubConfig
  -> (Connection -> t -> IO (Either Text b))
  -> ScottyTypes.ActionT LT.Text IO ()
postWithAdminToken connection_data github_config f = do
    body_json <- S.jsonData
    maybe_auth_header <- S.header "token"

    insertion_result <- liftIO $ runExceptT $ do
        auth_token <- except $ maybeToEither (T.pack "Need \"token\" header!") maybe_auth_header
        when (LT.toStrict auth_token /= AuthConfig.admin_password github_config) $
          except $ Left $ T.pack "Incorrect admin password"

        ExceptT $ do
          conn <- liftIO $ DbHelpers.get_connection connection_data
          f conn body_json

    S.json $ WebApi.toJsonEither insertion_result


postWithAuthentication connection_data github_config session f = do

  func <- f
  let callback_func user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func $ SqlRead.AuthConnection conn user_alias

  rq <- S.request
  insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
  S.json $ DbInsertion.toInsertionResponse github_config insertion_result


rescanCommitCallback github_config commit = do
  SqlRead.AuthConnection conn user_alias <- ask

  liftIO $ do
    maybe_previously_posted_status <- SqlRead.getPostedGithubStatus
      conn
      owned_repo
      commit

    run_result <- runExceptT $
      StatusUpdate.handleFailedStatuses
        conn
        (AuthConfig.personal_access_token github_config)
        (Just user_alias)
        owned_repo
        commit
        maybe_previously_posted_status

    return $ first LT.toStrict $ run_result $> 0

  where
    owned_repo = DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name


scottyApp ::
     PersistenceData
  -> SetupData
  -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp
    (PersistenceData cache session store)
    (SetupData static_base github_config connection_data) = do

  S.middleware $ withSession store (fromString "SESSION") def session

  S.middleware $ staticPolicy $ noDots >-> addBase static_base

  unless (AuthConfig.no_force_ssl github_config || AuthConfig.is_local github_config) $
    S.middleware forceSSL


  -- For debugging only
  when (AuthConfig.is_local github_config) echoEndpoint


  S.post "/api/github-event" $ StatusUpdate.githubEventEndpoint connection_data github_config

  S.post "/api/code-breakage-resolution-report" $
    breakageResolutionReport connection_data session github_config

  S.post "/api/code-breakage-cause-report" $
    breakageCauseReport connection_data session github_config

  S.post "/api/rescan-build" $ do
    postWithAuthentication connection_data github_config session $
      Scanning.rescanSingleBuildWrapped
        <$> (Builds.UniversalBuildId <$> S.param "build")

  S.post "/api/rescan-commit" $ do
    postWithAuthentication connection_data github_config session $
      rescanCommitCallback github_config
        <$> (Builds.RawCommit <$> S.param "sha1")

  S.post "/api/populate-master-commits" $
    postWithAdminToken connection_data github_config SqlWrite.storeMasterCommits

  S.post "/api/populate-master-commit-metadata" $
    postWithAdminToken connection_data github_config SqlWrite.storeCommitMetadata


    -- TODO FINISH ME
    {-
    S.post "/api/new-pattern-replace" $ do

      new_pattern <- patternFromParms
      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            SqlWrite.copy_pattern xxx

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ DbInsertion.toInsertionResponse github_config insertion_result
    -}


  S.post "/api/new-pattern-insert" $
    postWithAuthentication connection_data github_config session $
      SqlWrite.apiNewPatternWrapped <$> patternFromParms


  S.post "/api/code-breakage-mode-update" $
    postWithAuthentication connection_data github_config session $
      SqlWrite.updateCodeBreakageMode
        <$> S.param "cause_id"
        <*> S.param "mode"



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


  get "/api/latest-master-commit-with-metadata" $
    pure $ WebApi.toJsonEither <$> SqlRead.getLatestMasterCommitWithMetadata

  get "/api/status-posted-commits-by-day" $
    pure SqlRead.apiStatusPostedCommitsByDay

  get "/api/status-postings-by-day" $
    pure SqlRead.apiStatusPostingsByDay

  get "/api/failed-commits-by-day" $
    pure SqlRead.apiFailedCommitsByDay

  get "/api/code-breakages-leftover-by-commit" $
    pure SqlRead.apiLeftoverCodeBreakagesByCommit

  get "/api/code-breakages-leftover-detected" $
    pure SqlRead.apiLeftoverDetectedCodeBreakages

  get "/api/code-breakages-detected" $
    pure SqlRead.apiDetectedCodeBreakages

  get "/api/code-breakages-annotated" $
    pure SqlRead.apiAnnotatedCodeBreakages

  get "/api/list-failure-modes" $
    pure SqlRead.apiListFailureModes

  get "/api/job" $
    pure SqlRead.apiJobs

  get "/api/log-storage-stats" $
    pure SqlRead.apiStorageStats

  get "/api/log-size-histogram" $
    pure SqlRead.apiByteCountHistogram

  get "/api/log-lines-histogram" $
    pure SqlRead.apiLineCountHistogram

  get "/api/master-build-stats" $
    pure SqlRead.masterBuildFailureStats

  get "/api/patterns-dump" $
    pure SqlRead.dumpPatterns

  get "/api/patterns-timeline" $
    pure SqlRead.apiPatternOccurrenceTimeline

  get "/api/patterns" $
    pure SqlRead.apiPatterns

  get "/api/presumed-stable-branches-dump" $
    pure SqlRead.dumpPresumedStableBranches

  get "/api/step-list" $
    pure SqlRead.apiListSteps

  get "/api/step" $
    pure SqlRead.apiStep

  get "/api/master-deterministic-failure-modes" $
    pure SqlRead.apiDeterministicFailureModes

  get "/api/summary" $
    pure SqlRead.apiSummaryStats

  get "/api/tags" $
    pure SqlRead.apiTagsHistogram

  get "/api/unmatched-builds" $
    pure SqlRead.apiUnmatchedBuilds

  get "/api/idiopathic-failed-builds" $
    pure SqlRead.apiIdiopathicBuilds

  get "/api/patterns-presumed-stable-branches" $
    pure SqlRead.apiPatternsPresumedStableBranches

  get "/api/patterns-branch-filtered" $
    SqlRead.apiPatternsBranchFiltered <$> S.param "branches"

  get "/api/posted-statuses" $
    SqlRead.apiPostedStatuses <$> S.param "count"

  get "/api/aggregate-posted-statuses" $
    SqlRead.apiAggregatePostedStatuses <$> S.param "count"

  get "/api/known-breakage-affected-jobs" $
    SqlRead.knownBreakageAffectedJobs <$> S.param "cause_id"

  get "/api/tag-suggest" $
    SqlRead.apiAutocompleteTags <$> S.param "term"

  get "/api/step-suggest" $
    SqlRead.apiAutocompleteSteps <$> S.param "term"

  get "/api/branch-suggest" $
    SqlRead.apiAutocompleteBranches <$> S.param "term"

  get "/api/downstream-impact-weekly" $
    SqlRead.downstreamWeeklyFailureStats <$> S.param "weeks"

  get "/api/master-weekly-failure-stats" $
    SqlRead.masterWeeklyFailureStats <$> S.param "weeks"

  get "/api/unmatched-builds-for-commit" $
    SqlRead.apiUnmatchedCommitBuilds <$> S.param "sha1"

  get "/api/idiopathic-failed-builds-for-commit" $
    SqlRead.apiIdiopathicCommitBuilds <$> S.param "sha1"

  get "/api/timed-out-builds-for-commit" $
    SqlRead.apiTimeoutCommitBuilds <$> S.param "sha1"

  get "/api/pattern-step-occurrences" $
    SqlRead.patternBuildStepOccurrences . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/pattern-job-occurrences" $
    SqlRead.patternBuildJobOccurrences . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/list-commit-jobs" $
    SqlRead.apiCommitJobs . Builds.RawCommit <$> S.param "sha1"

  get "/api/build-pattern-matches" $
    SqlRead.getBuildPatternMatches . Builds.UniversalBuildId <$> S.param "build_id"

  get "/api/best-pattern-matches" $
    SqlRead.getBestPatternMatches . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/pattern-matches" $
    SqlRead.getPatternMatches . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/pattern" $
    SqlRead.apiSinglePattern . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/best-build-match" $
    SqlRead.getBestBuildMatch . Builds.UniversalBuildId <$> S.param "build_id"

  get "/api/test-failures" $
    fmap WebApi.toJsonEither . SqlRead.apiTestFailures . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/single-build-info" $
    fmap WebApi.toJsonEither . SqlUpdate.getBuildInfo (AuthConfig.personal_access_token github_config) . Builds.UniversalBuildId
    <$> S.param "build_id"

  get "/api/list-master-commit-range-jobs" $
    fmap SqlRead.apiCommitRangeJobs $ SqlRead.InclusiveSpan
      <$> S.param "first_index"
      <*> S.param "last_index"

  get "/api/universal-builds" $
    SqlRead.getUniversalBuilds
      <$> (Builds.UniversalBuildId <$> S.param "start-id")
      <*> S.param "limit"

  get "/api/master-timeline" $
    SqlRead.apiMasterBuilds <$> getOffsetMode

  S.get "/api/commit-info" $ do
    commit_sha1_text <- S.param "sha1"
    json_result <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      runExceptT $ do
        sha1 <- except $ GitRev.validateSha1 commit_sha1_text
        ExceptT $ runReaderT (SqlUpdate.countRevisionBuilds (AuthConfig.personal_access_token github_config) sha1) conn

    S.json $ WebApi.toJsonEither json_result

  S.get "/api/commit-builds" $ do
    commit_sha1_text <- S.param "sha1"
    json_result <- runExceptT $ do
      sha1 <- except $ GitRev.validateSha1 commit_sha1_text
      liftIO $ do
        conn <- DbHelpers.get_connection connection_data
        runReaderT (SqlRead.getRevisionBuilds sha1) conn

    S.json $ WebApi.toJsonEither json_result

  S.get "/api/new-pattern-test" $ do
    buildnum <- S.param "build_num"
    new_pattern <- patternFromParms

    x <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      runReaderT (SqlRead.apiNewPatternTest (Builds.UniversalBuildId buildnum) new_pattern) conn
    S.json $ WebApi.toJsonEither x


  -- Access-controlled GET endpoint
  S.get "/api/view-log-context" $
    jsonAuthorizedDbInteract connection_data session github_config $
      SqlRead.logContextFunc
        <$> (MatchOccurrences.MatchId <$> S.param "match_id")
        <*> S.param "context_linecount"


  S.get "/api/view-log-full" $ do
    build_id <- S.param "build_id"

    let callback_func :: AuthStages.Username -> IO (Either Text Text)

        universal_build_id = Builds.UniversalBuildId build_id

        callback_func _user_alias = do
          conn <- DbHelpers.get_connection connection_data

          putStrLn "getting global build"
          storable_build <- SqlRead.getGlobalBuild conn universal_build_id
          putStrLn "got global build"

          maybe_log <- SqlRead.readLog conn $ Builds.UniversalBuildId $ DbHelpers.db_id $ Builds.universal_build storable_build
          return $ maybeToEither "log not in database" maybe_log

    rq <- S.request
    either_log_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
    case either_log_result of
      Right logs  -> S.text $ LT.fromStrict logs
      Left errors -> S.html $ LT.fromStrict $ JsonUtils._message $ JsonUtils.getDetails errors

  post "/api/pattern-specificity-update" $
    SqlWrite.updatePatternSpecificity
      <$> (ScanPatterns.PatternId <$> S.param "pattern_id")
      <*> S.param "specificity"

  post "/api/pattern-description-update" $
    SqlWrite.updatePatternDescription
      <$> (ScanPatterns.PatternId <$> S.param "pattern_id")
      <*> S.param "description"

  post "/api/pattern-tag-add" $
    SqlWrite.addPatternTag
      <$> (ScanPatterns.PatternId <$> S.param "pattern_id")
      <*> S.param "tag"

  post "/api/pattern-tag-remove" $
    SqlWrite.removePatternTag
      <$> (ScanPatterns.PatternId <$> S.param "pattern_id")
      <*> S.param "tag"

  post "/api/patterns-restore" $
    SqlWrite.restorePatterns
      <$> S.jsonData

  post "/api/code-breakage-job-delete" $
    SqlWrite.deleteCodeBreakageJob
      <$> S.param "cause_id"
      <*> S.param "job"

  post "/api/code-breakage-description-update" $
    SqlWrite.updateCodeBreakageDescription
      <$> S.param "cause_id"
      <*> S.param "description"

  post "/api/code-breakage-delete" $
    SqlWrite.deleteCodeBreakage
      <$> S.param "cause_id"


  S.get "/favicon.ico" $ do
    S.setHeader "Content-Type" "image/x-icon"
    S.file $ static_base </> "images/favicon.ico"

  S.options "/" $ do
    S.setHeader "Access-Control-Allow-Origin" "*"
    S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

  S.get "/" $ do
    S.setHeader "Content-Type" "text/html; charset=utf-8"
    S.file $ static_base </> "index.html"

  where
    get x = jsonDbGet connection_data x

    post x y = S.post x $
      jsonAuthorizedDbInteract connection_data session github_config y

