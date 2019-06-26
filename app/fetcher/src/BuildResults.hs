{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BuildResults where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics
import           GHC.Int          (Int64)

import qualified Builds
import qualified DbHelpers
import qualified JsonUtils
import qualified MatchOccurrences


type IndexedCommit = DbHelpers.WithId Builds.RawCommit


data FailureMode =
   Timeout
 | NoLog
 | PatternMatch MatchOccurrences.MatchOccurrencesForBuild
 deriving Generic

instance ToJSON FailureMode


data SimpleBuildStatus = SimpleBuildStatus {
    _build        :: Builds.Build
  , _failure_mode :: FailureMode
  , _is_flaky     :: Bool
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


data BreakageEnd a = BreakageEnd {
    _resolution_commit :: IndexedCommit
  , _cause_id          :: Int64
  , _affected_jobs     :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (BreakageEnd a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore

type BreakageEndRecord a = DbHelpers.WithId (DbHelpers.WithAuthorship (BreakageEnd a))


data BreakageSpan a = BreakageSpan {
    _start :: DbHelpers.WithId (DbHelpers.WithAuthorship (BreakageStart a))
  , _end   :: Maybe (BreakageEndRecord a)
  } deriving Generic

instance (ToJSON a) => ToJSON (BreakageSpan a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data MasterBuildsResponse = MasterBuildsResponse {
    _columns        :: [Text]
  , _commits        :: [IndexedCommit]
  , _failures       :: [SimpleBuildStatus]
  , _breakage_spans :: [BreakageSpan (DbHelpers.WithAuthorship Text)]
  } deriving Generic

instance ToJSON MasterBuildsResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore
