{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module BuildResults where

import           Data.Aeson                         hiding (Success)
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple         (FromRow)
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           GHC.Generics
import           GHC.Int                            (Int64)

import qualified Builds
import qualified Commits
import qualified DbHelpers
import qualified JsonUtils
import qualified MatchOccurrences


type IndexedCommit = DbHelpers.WithId Builds.RawCommit

type IndexedRichCommit = DbHelpers.WithId CommitAndMetadata


data CommitAndMetadata = CommitAndMetadata {
    _commit   :: Builds.RawCommit
  , _metadata :: Maybe Commits.CommitMetadata
  } deriving Generic

instance ToJSON CommitAndMetadata where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data StepFailure =
    Timeout
  | NoMatch -- ^ no pattern match
  | PatternMatch MatchOccurrences.MatchOccurrencesForBuild
  deriving Generic

instance ToJSON StepFailure


isSuccess :: FailureMode -> Bool
isSuccess Success = True
isSuccess _       = False

data FailureMode =
   Success
 | NoLog
 | FailedStep {
       _step_name    :: Text
     , _step_failure :: StepFailure
     }
 | Unknown -- ^ this should never happen
 deriving Generic

instance ToJSON FailureMode where
  toJSON = genericToJSON JsonUtils.dropUnderscore


readMaybeLongitudinalCluster = do
  contiguous_run_count <- field
  contiguous_group_index <- field
  contiguous_start_commit_index <- field
  contiguous_end_commit_index <- field
  contiguous_length <- field

  return $ BuildResults.ContiguousBreakageMember
    <$> contiguous_run_count
    <*> contiguous_group_index
    <*> contiguous_start_commit_index
    <*> contiguous_end_commit_index
    <*> contiguous_length


readMaybeLateralCluster = do
  maybe_cluster_id <- field
  maybe_cluster_member_count <- field

  return $ BuildResults.LateralBreakageMember
    <$> maybe_cluster_member_count
    <*> maybe_cluster_id


data DetectedBreakageModes = DetectedBreakageModes {
    _longitudinal_breakage :: Maybe ContiguousBreakageMember
  , _lateral_breakage      :: Maybe LateralBreakageMember
  } deriving Generic

instance ToJSON DetectedBreakageModes where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromRow DetectedBreakageModes where
  fromRow = do

    maybe_contiguous_member <- readMaybeLongitudinalCluster
    maybe_lateral_breakage <- readMaybeLateralCluster

    return $ DetectedBreakageModes
      maybe_contiguous_member
      maybe_lateral_breakage


data SimpleBuildStatus = SimpleBuildStatus {
    _build               :: Builds.Build
  , _failure_mode        :: FailureMode
  , _is_flaky            :: Bool
  , _is_known_broken     :: Bool
  , _detected_breakages  :: DetectedBreakageModes
  , _is_isolated_failure :: Bool
  , _universal_build     :: DbHelpers.WithId Builds.UniversalBuild
--  , _ci_provider         :: DbHelpers.WithId Builds.CiProvider
  } deriving Generic

instance ToJSON SimpleBuildStatus where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data MasterFailureModeDetails = MasterFailureModeDetails {
    _label      :: Text
  , _revertible :: Bool
  } deriving (Generic, FromRow)

instance ToJSON MasterFailureModeDetails where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageStart a = BreakageStart {
    _breakage_commit :: IndexedCommit
  , _description     :: Text
  , _breakage_mode   :: DbHelpers.WithAuthorship Int64
  , _affected_jobs   :: [a]
  , _metadata        :: DbHelpers.WithAuthorship Text
  } deriving Generic

instance (ToJSON a) => ToJSON (BreakageStart a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageEnd = BreakageEnd {
    _resolution_commit :: IndexedCommit
  , _cause_id          :: Int64
  , _metadata          :: DbHelpers.WithAuthorship Text
  } deriving Generic

instance ToJSON BreakageEnd where
  toJSON = genericToJSON JsonUtils.dropUnderscore


type BreakageEndRecord = DbHelpers.WithId (DbHelpers.WithAuthorship BreakageEnd)


data WeeklyBreakageImpactStats = WeeklyBreakageImpactStats {
    _week           :: UTCTime
  , _incident_count :: Int
  , _impact         :: BreakageImpactStats
  } deriving Generic

instance ToJSON WeeklyBreakageImpactStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageImpactStats = BreakageImpactStats {
    _downstream_broken_commit_count :: Int
  , _failed_downstream_build_count  :: Int
  } deriving (Generic, FromRow)

instance ToJSON BreakageImpactStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageSpan a b = BreakageSpan {
    _start :: DbHelpers.WithId (DbHelpers.WithAuthorship (BreakageStart a))
  , _end   :: Maybe BreakageEndRecord
  , _spanned_commit_count :: Maybe Int
  , _impact_stats :: b
  } deriving Generic

instance (ToJSON a, ToJSON b) => ToJSON (BreakageSpan a b) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


maybeResolutionFromRow = do

  maybe_resolution_id <- field
  maybe_resolved_commit_index <- field
  maybe_resolution_sha1 <- field
  maybe_resolution_reporter <- field
  maybe_resolution_reported_at <- field

  resolution_commit_author <- field
  resolution_commit_message <- field
  resolution_commit_date <- field

  return $ do
    resolution_id <- maybe_resolution_id
    resolved_commit_index <- maybe_resolved_commit_index
    resolution_sha1 <- maybe_resolution_sha1
    resolution_reporter <- maybe_resolution_reporter
    resolution_reported_at <- maybe_resolution_reported_at

    let end_commit = DbHelpers.WithId resolved_commit_index $ Builds.RawCommit resolution_sha1
        end_record = DbHelpers.WithId resolution_id $ DbHelpers.WithAuthorship resolution_reporter resolution_reported_at $ BuildResults.BreakageEnd end_commit resolution_id $ DbHelpers.WithAuthorship resolution_commit_author resolution_commit_date resolution_commit_message

    return end_record


causeFromRow = do

  cause_id <- field
  cause_commit_index <- field
  cause_sha1 <- field
  description <- field
  failure_mode_reporter <- field
  failure_mode_reported_at <- field
  failure_mode_id <- field
  cause_reporter <- field
  cause_reported_at <- field
  cause_jobs_delimited <- field

  breakage_commit_author <- field
  breakage_commit_message <- field
  breakage_commit_date <- field

  let cause_commit_metadata = DbHelpers.WithAuthorship breakage_commit_author breakage_commit_date breakage_commit_message
      breakage_start =
        BuildResults.BreakageStart
          (DbHelpers.WithId cause_commit_index $ Builds.RawCommit cause_sha1)
          description
          (DbHelpers.WithAuthorship failure_mode_reporter failure_mode_reported_at failure_mode_id)
          (map T.pack $ DbHelpers.splitAggText (cause_jobs_delimited :: String))
          cause_commit_metadata

  return $ DbHelpers.WithId cause_id $
    DbHelpers.WithAuthorship cause_reporter cause_reported_at breakage_start


instance FromRow (BreakageSpan Text BreakageImpactStats) where
  fromRow = do

    cause <- causeFromRow
    maybe_resolution <- maybeResolutionFromRow
    spanned_commit_count <- field

    impact_stats <- fromRow

    return $ BreakageSpan
      cause
      maybe_resolution
      spanned_commit_count
      impact_stats


instance FromRow (BreakageSpan Text ()) where
  fromRow = do

    cause <- causeFromRow
    maybe_resolution <- maybeResolutionFromRow
    spanned_commit_count <- field

    return $ BreakageSpan
      cause
      maybe_resolution
      spanned_commit_count
      ()


data DbMasterBuildsBenchmarks = DbMasterBuildsBenchmarks {
    _builds_list_time    :: Float
  , _commits_list_time   :: Float
  , _code_breakages_time :: Float
  } deriving Generic

instance ToJSON DbMasterBuildsBenchmarks where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data MasterBuildsResponse = MasterBuildsResponse {
    _columns         :: Set Text
  , _commits         :: [IndexedRichCommit]
  , _failures        :: [SimpleBuildStatus] -- ^ also includes successes
  , _breakage_spans  :: [BreakageSpan Text ()]
  , _db_benchmarking :: DbMasterBuildsBenchmarks
  } deriving Generic

instance ToJSON MasterBuildsResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data ContiguousBreakageMember = ContiguousBreakageMember {
    _contiguous_run_count          :: Int
  , _contiguous_group_index        :: Int
  , _contiguous_start_commit_index :: Int
  , _contiguous_end_commit_index   :: Int
  , _contiguous_length             :: Int
  } deriving Generic

instance ToJSON ContiguousBreakageMember where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data LateralBreakageMember = LateralBreakageMember {
    _cluster_member_count :: Int
  , _cluster_id           :: Int
  } deriving Generic

instance ToJSON LateralBreakageMember where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data DetectedBreakageSpan = DetectedBreakageSpan {
    _first_commit_id      :: Int
  , _jobs_delimited       :: Text
  , _job_count            :: Int
  , _min_run_length       :: Int
  , _max_run_length       :: Int
  , _modal_run_length     :: Int
  , _min_last_commit_id   :: Int
  , _max_last_commit_id   :: Int
  , _modal_last_commit_id :: Int
  , _first_commit         :: Text
  , _min_last_commit      :: Text
  , _max_last_commit      :: Text
  , _modal_last_commit    :: Text
  } deriving (Generic, FromRow)

instance ToJSON DetectedBreakageSpan where
  toJSON = genericToJSON JsonUtils.dropUnderscore
