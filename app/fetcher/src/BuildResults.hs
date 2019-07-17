{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BuildResults where

import           Data.Aeson
import           Data.Set         (Set)
import           Data.Text        (Text)
import           GHC.Generics
import           GHC.Int          (Int64)

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
    _build           :: Builds.Build
  , _failure_mode    :: FailureMode
  , _is_flaky        :: Bool
  , _is_known_broken :: Bool
  } deriving Generic

instance ToJSON SimpleBuildStatus where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageStart a = BreakageStart {
    _breakage_commit :: IndexedCommit
  , _description     :: Text
  , _affected_jobs   :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (BreakageStart a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageEnd = BreakageEnd {
    _resolution_commit :: IndexedCommit
  , _cause_id          :: Int64
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
  , _breakage_spans :: [BreakageSpan (DbHelpers.WithAuthorship Text)]
  } deriving Generic

instance ToJSON MasterBuildsResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore
