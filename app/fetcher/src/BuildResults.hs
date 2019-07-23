{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module BuildResults where

import           Data.Aeson
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
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


data SimpleBuildStatus = SimpleBuildStatus {
    _build               :: Builds.Build
  , _failure_mode        :: FailureMode
  , _is_flaky            :: Bool
  , _is_known_broken     :: Bool
  , _contiguous_breakage :: Maybe ContiguousBreakageMember
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
  , _failure_mode    :: DbHelpers.WithAuthorship Int64
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


data BreakageSpan a = BreakageSpan {
    _start :: DbHelpers.WithId (DbHelpers.WithAuthorship (BreakageStart a))
  , _end   :: Maybe BreakageEndRecord
  } deriving Generic

instance (ToJSON a) => ToJSON (BreakageSpan a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


instance FromRow (BreakageSpan Text) where
  fromRow = do
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
    maybe_resolution_id <- field
    maybe_resolved_commit_index <- field
    maybe_resolution_sha1 <- field
    maybe_resolution_reporter <- field
    maybe_resolution_reported_at <- field
    breakage_commit_author <- field
    breakage_commit_message <- field
    resolution_commit_author <- field
    resolution_commit_message <- field
    breakage_commit_date <- field
    resolution_commit_date <- field

    let cause_commit_metadata = DbHelpers.WithAuthorship breakage_commit_author breakage_commit_date breakage_commit_message
        cause = DbHelpers.WithId cause_id $ DbHelpers.WithAuthorship cause_reporter cause_reported_at $
          BuildResults.BreakageStart
            (DbHelpers.WithId cause_commit_index $ Builds.RawCommit cause_sha1)
            description
            (DbHelpers.WithAuthorship failure_mode_reporter failure_mode_reported_at failure_mode_id)
            (map T.pack $ DbHelpers.splitAggText (cause_jobs_delimited :: String))
            cause_commit_metadata

        maybe_resolution = do
          resolution_id <- maybe_resolution_id
          resolved_commit_index <- maybe_resolved_commit_index
          resolution_sha1 <- maybe_resolution_sha1
          resolution_reporter <- maybe_resolution_reporter
          resolution_reported_at <- maybe_resolution_reported_at

          let end_commit = DbHelpers.WithId resolved_commit_index $ Builds.RawCommit resolution_sha1
              end_record = DbHelpers.WithId resolution_id $ DbHelpers.WithAuthorship resolution_reporter resolution_reported_at $ BuildResults.BreakageEnd end_commit resolution_id $ DbHelpers.WithAuthorship resolution_commit_author resolution_commit_date resolution_commit_message

          return end_record

    return $ BuildResults.BreakageSpan cause maybe_resolution


data MasterBuildsResponse = MasterBuildsResponse {
    _columns        :: Set Text
  , _commits        :: [IndexedRichCommit]
  , _failures       :: [SimpleBuildStatus]
  , _breakage_spans :: [BreakageSpan Text]
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
