{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT (ExceptT), except,
                                                  runExceptT)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Default                    (def)
import           Data.Either.Utils               (maybeToEither)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as LT
import qualified Data.Vault.Lazy                 as Vault
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
import qualified DbHelpers
import qualified FrontendHelpers
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusUpdate
import qualified Types
import qualified WebApi


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
  when (AuthConfig.is_local github_config) FrontendHelpers.echoEndpoint


  S.post "/api/github-event" $ StatusUpdate.githubEventEndpoint connection_data github_config

  S.post "/api/code-breakage-resolution-report" $
    FrontendHelpers.breakageResolutionReport connection_data session github_config

  S.post "/api/code-breakage-cause-report" $
    FrontendHelpers.breakageCauseReport connection_data session github_config

  S.post "/api/rescan-build" $
    withAuth $
      Scanning.rescanSingleBuildWrapped
        <$> (Builds.UniversalBuildId <$> S.param "build")

  S.post "/api/rescan-commit" $
    withAuth $
      FrontendHelpers.rescanCommitCallback github_config
        <$> (Builds.RawCommit <$> S.param "sha1")

  S.post "/api/populate-master-commits" $
    FrontendHelpers.postWithAdminToken connection_data github_config SqlWrite.storeMasterCommits

  S.post "/api/populate-master-commit-metadata" $
    FrontendHelpers.postWithAdminToken connection_data github_config SqlWrite.storeCommitMetadata


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
    withAuth $
      SqlWrite.apiNewPatternWrapped <$> FrontendHelpers.patternFromParms


  S.post "/api/code-breakage-mode-update" $
    withAuth $
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
    fmap WebApi.toJsonEither . SqlRead.apiMasterBuilds <$> FrontendHelpers.getOffsetMode

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
    new_pattern <- FrontendHelpers.patternFromParms

    x <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      runReaderT (SqlRead.apiNewPatternTest (Builds.UniversalBuildId buildnum) new_pattern) conn
    S.json $ WebApi.toJsonEither x


  -- Access-controlled GET endpoint
  S.get "/api/view-log-context" $
    FrontendHelpers.jsonAuthorizedDbInteract connection_data session github_config $
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
    get x = FrontendHelpers.jsonDbGet connection_data x

    post x y = S.post x $
      FrontendHelpers.jsonAuthorizedDbInteract connection_data session github_config y

    withAuth = FrontendHelpers.postWithAuthentication connection_data github_config session
