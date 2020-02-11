{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT (ExceptT), except,
                                                  runExceptT)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Aeson                      (ToJSON)
import           Data.Default                    (def)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as LT
import qualified Data.Vault.Lazy                 as Vault
import           Log                             (LogT, localDomain)
import           Network.Wai
import           Network.Wai.Log                 (logRequestsWith)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.Gzip     (gzip)
import           Network.Wai.Middleware.Static   hiding ((<|>))
import           Network.Wai.Session             (Session, SessionStore,
                                                  withSession)
import           System.FilePath
import qualified Web.Scotty                      as S
import qualified Web.Scotty.Internal.Types       as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified BuildRetrieval
import qualified Builds
import qualified CircleApi
import qualified Constants
import qualified DbHelpers
import qualified DebugUtils                      as D
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
    _setup_static_base           :: String
  , _setup_github_config         :: AuthConfig.GithubConfig
  , _third_party_creds           :: CircleApi.ThirdPartyAuth
  , _setup_connection_data       :: DbHelpers.DbConnectionData
  , _setup_mview_connection_data :: DbHelpers.DbConnectionData -- ^ for updating materialized views
  }


data PersistenceData = PersistenceData {
    _setup_cache   :: Types.CacheStore
  , _setup_session :: Vault.Key (Session IO String String)
  , _setup_store   :: SessionStore IO String String
  }


scottyApp ::
     (LogT IO () -> IO ())
  -> PersistenceData
  -> SetupData
  -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp
    logger
    (PersistenceData cache session store)
    (SetupData static_base github_config third_party_creds connection_data mview_connection_data) = do

  S.middleware $ logRequestsWith $
    \logger_t -> logger $ localDomain logger_domain_identifier logger_t

  S.middleware $ withSession store (fromString "SESSION") def session

  S.middleware $ staticPolicy $ noDots >-> addBase static_base

  S.middleware $ gzip def

  unless (AuthConfig.no_force_ssl github_config || AuthConfig.is_local github_config) $
    S.middleware forceSSL


  -- For debugging only
  when (AuthConfig.is_local github_config) FrontendHelpers.echoEndpoint


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


  S.post "/api/github-event" $ StatusUpdate.githubEventEndpoint $
    Constants.ProviderConfigs
      github_config
      third_party_creds
      connection_data


  S.post "/api/code-breakage-resolution-report" $
    FrontendHelpers.breakageResolutionReport
      (FrontendHelpers.AuthHelperBundle connection_data session github_config third_party_creds)
      mview_connection_data

  S.post "/api/code-breakage-cause-report" $
    FrontendHelpers.breakageCauseReport
      (FrontendHelpers.AuthHelperBundle connection_data session github_config third_party_creds)
      mview_connection_data

  S.post "/api/refresh-materialized-view" $ do
    view_name <- S.param "view-name"
    is_from_frontend <- S.param "from-frontend"

    result <- liftIO $ do
      mview_conn <- DbHelpers.get_connection mview_connection_data
      flip runReaderT mview_conn $ SqlRead.refreshCachedMasterGrid view_name $ FrontendHelpers.checkboxIsTrue is_from_frontend

    S.json $ WebApi.toJsonEither result

  S.post "/api/get-logged-in-user" $
    withAuth $ pure FrontendHelpers.getLoggedInUser

  S.post "/api/rescan-multiple-builds" $
    withAuth $
      Scanning.apiRescanBuilds third_party_creds
        <$> S.jsonData

  S.post "/api/rescan-build" $
    withAuth $
      Scanning.rescanSingleBuildWrapped third_party_creds
        <$> (Builds.UniversalBuildId <$> S.param "build")

  S.post "/api/rebuild-single-job" $
    withAuth $ FrontendHelpers.facilitateJobRebuild (CircleApi.circle_api_token third_party_creds)
      <$> (Builds.UniversalBuildId <$> S.param "build")

  S.post "/api/promote-match" $
    withAuth $
      SqlWrite.promoteMatch
        <$> (MatchOccurrences.MatchId <$> S.param "match_id")

  S.post "/api/elaborate-failure" $
    withAuth $
      SqlWrite.elaborateBuildFailure
        <$> (Builds.UniversalBuildId <$> S.param "build")

  S.post "/api/rescan-commit" $
    withAuth $
      FrontendHelpers.rescanCommitCallback third_party_creds
        <$> (Builds.RawCommit <$> S.param "sha1")

  S.post "/api/populate-master-commits" $
    FrontendHelpers.requireAdminToken connection_data github_config SqlWrite.storeMasterCommits

  S.post "/api/populate-master-commit-metadata" $
    FrontendHelpers.requireAdminToken connection_data github_config SqlWrite.storeCommitMetadata

  S.post "/api/new-pattern-insert" $
    withAuth $
      SqlWrite.apiNewPatternWrapped <$> FrontendHelpers.patternFromParms

  S.post "/api/code-breakage-mode-update" $
    withAuth $
      SqlWrite.updateCodeBreakageMode
        <$> S.param "cause_id"
        <*> S.param "mode"

  S.post "/api/create-pattern-remediation" $
    withAuth $
      SqlWrite.createPatternRemediation
        <$> (ScanPatterns.PatternId <$> S.param "pattern_id")
        <*> parseRemediationObject

  get "/api/latest-master-commit-with-metadata" $
    pure $ WebApi.toJsonEither <$> SqlRead.getLatestMasterCommitWithMetadata

  get "/api/status-notifications-by-hour" $
    SqlRead.apiStatusNotificationsByHour <$> S.param "hours"

  get "/api/master-downstream-commits" $
    SqlRead.apiMasterDownstreamCommits . Builds.RawCommit <$> S.param "sha1"

  get "/api/status-postings-by-day" $
    pure SqlRead.apiStatusPostingsByDay

  get "/api/comment-postings-by-week" $
    pure SqlRead.prCommentRevisionsByWeek

  get "/api/failed-commits-by-day" $
    pure SqlRead.apiFailedCommitsByDay

  get "/api/code-breakages-leftover-by-commit" $
    pure SqlRead.apiLeftoverCodeBreakagesByCommit

  get "/api/missing-required-builds" $
    pure SqlRead.apiMissingRequiredBuilds

  get "/api/scan-commits-queue" $
    pure SqlRead.apiScanCommitsQueue

  get "/api/code-breakages-leftover-detected" $
    pure SqlRead.apiLeftoverDetectedCodeBreakages

  get "/api/code-breakages-detected" $
    pure SqlRead.apiDetectedCodeBreakages

  get "/api/code-breakages-annotated" $
    pure SqlRead.apiAnnotatedCodeBreakagesWithImpact

  get "/api/code-breakages-author-stats" $
    pure SqlRead.apiBreakageAuthorStats

  get "/api/broken-commits-without-metadata" $
    pure SqlRead.apiBrokenCommitsWithoutMetadata

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

  -- FIXME This is legacy. Don't use this!
  get "/api/inferred-scheduled-builds" $
    pure SqlRead.getScheduledJobNames

  get "/api/step-list" $
    pure SqlRead.apiListSteps

  get "/api/step" $
    pure SqlRead.apiStep

  get "/api/master-commits-granular" $
    pure SqlRead.masterCommitsGranular

  get "/api/master-deterministic-failure-modes" $
    pure SqlRead.apiDeterministicFailureModes

  get "/api/mview-refreshes" $
    pure SqlRead.apiMaterializedViewRefreshes

  get "/api/summary" $
    pure SqlRead.apiSummaryStats

  get "/api/job-schedule-stats" $
    pure SqlRead.apiJobScheduleStats

  get "/api/tags" $
    pure SqlRead.apiTagsHistogram

  get "/api/unmatched-builds" $
    pure SqlRead.apiUnmatchedBuilds

  get "/api/idiopathic-failed-builds" $
    pure SqlRead.apiIdiopathicBuilds

  get "/api/code-breakages-annotated-single" $
    SqlRead.apiAnnotatedCodeBreakagesWithImpactSingle <$> S.param "cause_id"

  get "/api/code-breakage-mode-single" $
    SqlRead.apiCodeBreakagesModeSingle <$> S.param "cause_id"

  get "/api/posted-pr-comments" $
    SqlRead.apiPostedPRComments <$> S.param "count"

  get "/api/upstream-broken-jobs-for-commit" $
    SqlRead.getInferredSpanningBrokenJobsBetter . Builds.RawCommit <$> S.param "sha1"

  get "/api/known-breakage-affected-jobs" $
    SqlRead.knownBreakageAffectedJobs <$> S.param "cause_id"

  get "/api/tag-suggest" $
    SqlRead.apiAutocompleteTags <$> S.param "term"

  get "/api/step-suggest" $
    SqlRead.apiAutocompleteSteps <$> S.param "term"

  get "/api/downstream-impact-weekly" $
    SqlRead.downstreamWeeklyFailureStats <$> S.param "weeks"

  get "/api/master-breakages-monthly-stats" $
    pure SqlRead.masterBreakageMonthlyStats

  get "/api/master-weekly-failure-stats" $
    fmap WebApi.toJsonEither . SqlRead.masterWeeklyFailureStats <$> S.param "weeks"

  get "/api/master-pr-merge-time-weekly-failure-stats" $
    SqlRead.getMergeTimeFailingPullRequestBuildsByWeek <$> S.param "weeks"

  get "/api/page-views-by-week" $
    SqlRead.getPageViewsByWeek
      <$> S.param "weeks"

  get "/api/unmatched-builds-for-commit" $
    fmap WebApi.toJsonEither . SqlRead.apiUnmatchedCommitBuilds . Builds.RawCommit <$> S.param "sha1"

  get "/api/idiopathic-failed-builds-for-commit" $
    fmap WebApi.toJsonEither . SqlRead.apiIdiopathicCommitBuilds . Builds.RawCommit <$> S.param "sha1"

  get "/api/timed-out-builds-for-commit" $
    fmap WebApi.toJsonEither . SqlRead.apiTimeoutCommitBuilds . Builds.RawCommit <$> S.param "sha1"

  get "/api/pattern-step-occurrences" $
    SqlRead.patternBuildStepOccurrences . ScanPatterns.PatternId <$> S.param "pattern_id"

  get "/api/pattern-job-occurrences" $
    SqlRead.patternBuildJobOccurrences . ScanPatterns.PatternId <$> S.param "pattern_id"

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

  get "/api/single-build-info" $
    fmap WebApi.toJsonEither . SqlUpdate.getBuildInfo third_party_creds . Builds.UniversalBuildId
      <$> S.param "build_id"

  get "/api/list-build-github-status-events" $
    SqlRead.apiGitHubNotificationsForBuild
      <$> S.param "job"
      <*> (Builds.RawCommit <$> S.param "sha1")

  get "/api/list-commit-jobs" $
    SqlRead.apiCommitJobs . Builds.RawCommit <$> S.param "sha1"

  get "/api/list-master-commit-range-jobs" $
    fmap SqlRead.apiCommitRangeJobs $ SqlRead.InclusiveSpan
      <$> S.param "first_index"
      <*> S.param "last_index"

  get "/api/master-commits" $
    fmap WebApi.toJsonEither . ((fmap . fmap . fmap) snd SqlRead.getMasterCommits) <$> FrontendHelpers.getSimpleOffsetMode

  get "/api/master-timeline" $
    fmap WebApi.toJsonEither . SqlRead.apiMasterBuilds <$> FrontendHelpers.getOffsetMode

  S.get "/api/commit-info" $ do
    commit_sha1_text <- S.param "sha1"
    json_result <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      runExceptT $ do
        sha1 <- except $ GitRev.validateSha1 commit_sha1_text
        ExceptT $ flip runReaderT conn $
          SqlUpdate.countRevisionBuilds
            third_party_creds
            sha1

    S.json $ WebApi.toJsonEither json_result

  get "/api/isolated-master-failures-by-day" $
    fmap WebApi.toJsonEither . SqlRead.apiIsolatedMasterFailuresByDay
      <$> S.param "age-days"

  get "/api/isolated-failures-timespan-coarse-bins" $
    fmap WebApi.toJsonEither . SqlRead.apiCoarseBinsIsolatedJobFailuresTimespan
      <$> parseTimeRangeParms

  get "/api/isolated-unmatched-failed-builds-master-commit-range" $
    (fmap . fmap) WebApi.toJsonEither $ SqlRead.apiIsolatedUnmatchedBuildsMasterCommitRange
      <$> (DbHelpers.InclusiveNumericBounds <$> S.param "commit-id-min" <*> S.param "commit-id-max")

  get "/api/isolated-failures-timespan-by-job" $
    fmap WebApi.toJsonEither . SqlRead.apiIsolatedJobFailuresTimespan
      <$> parseTimeRangeParms

  get "/api/isolated-failures-timespan-by-pattern" $
    fmap WebApi.toJsonEither . SqlRead.apiIsolatedPatternFailuresTimespan
      <$> parseTimeRangeParms

  get "/api/master-job-failures-in-timespan" $
    (fmap . fmap) WebApi.toJsonEither $ SqlRead.apiJobFailuresInTimespan
      <$> S.param "job"
      <*> (DbHelpers.InclusiveNumericBounds <$> S.param "commit-id-min" <*> S.param "commit-id-max")

  get "/api/master-pattern-failures-in-timespan" $
    (fmap . fmap) WebApi.toJsonEither $ SqlRead.apiPatternFailuresInTimespan
      <$> (ScanPatterns.PatternId <$> S.param "pattern")
      <*> (DbHelpers.InclusiveNumericBounds <$> S.param "commit-id-min" <*> S.param "commit-id-max")

  get "/api/latest-viable-master-commits" $
    SqlRead.apiCleanestMasterCommits
      <$> S.param "missing-threshold"
      <*> S.param "failing-threshold"

  get "/api/pr-batch-list" $
    SqlRead.apiPrBatchList
      <$> (map (read . LT.unpack) . LT.splitOn "," <$> S.param "pr-numbers-delimited")

  get "/api/viable-commit-age-history" $
    SqlRead.apiLatestViableMasterCommitAgeHistory
      <$> S.param "weeks"
      <*> (BuildRetrieval.decodeUtcTimeString <$> S.param "end-timestamp")

  get "/api/viable-commit-lag-count-history" $
    SqlRead.apiLatestViableMasterCommitLagCountHistory
      <$> S.param "weeks"
      <*> (BuildRetrieval.decodeUtcTimeString <$> S.param "end-timestamp")

  get "/api/is-master-commit" $
    SqlRead.isMasterCommit . Builds.RawCommit <$> S.param "sha1"

  S.get "/api/commit-builds" $ do

    commit_sha1_text <- S.param "sha1"
    json_result <- runExceptT $ do
      sha1 <- except $ GitRev.validateSha1 commit_sha1_text
      either_result <- liftIO $ do
        conn <- DbHelpers.get_connection connection_data
        flip runReaderT conn $ SqlRead.getRevisionBuilds sha1
      except either_result

    S.json $ WebApi.toJsonEither json_result

  S.get "/api/new-pattern-test" $ do
    buildnum <- S.param "build_num"
    new_pattern <- FrontendHelpers.patternFromParms

    x <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      flip runReaderT conn $ SqlRead.apiNewPatternTest (Builds.UniversalBuildId buildnum) new_pattern

    S.json $ WebApi.toJsonEither x

  S.get "/api/get-user-opt-out-settings" $
    FrontendHelpers.jsonAuthorizedDbInteract2
      (FrontendHelpers.AuthHelperBundle connection_data session github_config third_party_creds) $
        pure SqlRead.userOptOutSettings

  S.post "/api/update-user-opt-out-settings" $
    withAuth $ SqlWrite.updateUserOptOutSettings <$> S.param "enabled"

  S.post "/api/purge-stale-work-queue-entries" $
    withAuth $ pure SqlWrite.deleteStaleSha1QueueEntries

  get "/api/view-log-context" $ (fmap . fmap) WebApi.toJsonEither $
    SqlRead.logContextFunc SqlRead.hiddenContextLinecount
      <$> (MatchOccurrences.MatchId <$> S.param "match_id")
      <*> S.param "context_linecount"

  S.get "/api/view-log-full" $ do
    build_id <- S.param "build_id"

    let universal_build_id = Builds.UniversalBuildId build_id

    either_log_result <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      flip runReaderT conn $ SqlRead.retrieveLogFromBuildId universal_build_id

    case either_log_result of
      Right logs  -> S.text logs
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

  post "/api/code-breakage-update-resolution-sha1" $
    SqlWrite.updateCodeBreakageResolutionSha1
      <$> S.param "cause_id"
      <*> (Builds.RawCommit <$> S.param "resolution_sha1")

  post "/api/code-breakage-update-cause-sha1" $
    SqlWrite.updateCodeBreakageCauseSha1
      <$> S.param "cause_id"
      <*> (Builds.RawCommit <$> S.param "cause_sha1")

  post "/api/code-breakage-delete-resolution" $
    SqlWrite.deleteCodeBreakageResolution
      <$> S.param "cause_id"

  post "/api/code-breakage-delete" $
    SqlWrite.deleteCodeBreakage
      <$> S.param "cause_id"

  S.post "/api/code-breakage-add-affected-job" $
    withAuth $
      SqlWrite.addCodeBreakageJobName
        <$> S.param "cause_id"
        <*> S.param "job_name"

  S.get "/favicon.ico" $ do
    S.setHeader "Content-Type" "image/x-icon"
    S.file $ static_base </> "images/favicon.ico"

  S.options "/" $ do
    S.setHeader "Access-Control-Allow-Origin" "*"
    S.setHeader "Access-Control-Allow-Methods" $ LT.intercalate ", " [
        "POST"
      , "GET"
      , "OPTIONS"
      ]

  S.get "/" $ do
    S.setHeader "Content-Type" "text/html; charset=utf-8"
    S.file $ static_base </> "index.html"

  where
    get x = FrontendHelpers.jsonDbGet connection_data x

    post x y = S.post x $
      FrontendHelpers.jsonAuthorizedDbInteract
        (FrontendHelpers.AuthHelperBundle connection_data session github_config third_party_creds)
        y

    withAuth :: ToJSON a =>
         ScottyTypes.ActionT LT.Text IO (SqlRead.AuthDbIO (Either Text a))
      -> ScottyTypes.ActionT LT.Text IO ()
    withAuth = FrontendHelpers.postWithAuthentication
      (FrontendHelpers.AuthHelperBundle connection_data session github_config third_party_creds)

    logger_domain_identifier = if AuthConfig.is_local github_config
      then "localhost"
      else "dr.pytorch.org"


parseRemediationObject :: ScottyTypes.ActionT LT.Text IO SqlWrite.FailureRemediation
parseRemediationObject = do

  notes <- S.param "notes"
  github_issue_number <- S.param "github_issue_number"
  info_url <- S.param "info_url"

  return $ SqlWrite.FailureRemediation
    (Just notes)
    (Just github_issue_number)
    (Just info_url)


parseTimeRangeParms :: ScottyTypes.ActionT LT.Text IO SqlRead.TimeRange
parseTimeRangeParms = do
  start_timestamp <- BuildRetrieval.decodeUtcTimeString <$> S.param "start-timestamp"

  let bounded_result = do
        end_timestamp <- BuildRetrieval.decodeUtcTimeString <$> S.param "end-timestamp"
        return $ SqlRead.Bounded $ DbHelpers.StartEnd start_timestamp end_timestamp

  bounded_result `S.rescue` (\_msg -> return $ SqlRead.StartOnly start_timestamp)




