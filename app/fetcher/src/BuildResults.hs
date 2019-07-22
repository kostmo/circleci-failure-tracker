{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BuildResults where

import           Data.Aeson
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (FromRow)
import           GHC.Generics
import           GHC.Int                    (Int64)

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
  , _failure_mode    :: Int64
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
