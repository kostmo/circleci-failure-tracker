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


newtype RawCommit = RawCommit Text
 deriving Generic

instance ToJSON RawCommit


type IndexedCommit = DbHelpers.WithId RawCommit


data FailureMode =
   Timeout
 | NoLog
 | PatternMatch MatchOccurrences.MatchOccurrencesForBuild
 deriving Generic

instance ToJSON FailureMode


data SimpleBuildStatus = SimpleBuildStatus {
    _build        :: Builds.Build
  , _failure_mode :: FailureMode
  } deriving Generic

instance ToJSON SimpleBuildStatus where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageStart = BreakageStart {
    _breakage_commit :: IndexedCommit
  , _description     :: Text
  , _affected_jobs   :: [DbHelpers.WithAuthorship Text]
  } deriving Generic

instance ToJSON BreakageStart where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BreakageEnd = BreakageEnd {
    _resolution_commit :: IndexedCommit
  , _cause_id          :: Int64
  , _affected_jobs     :: [DbHelpers.WithAuthorship Text]
  } deriving Generic

instance ToJSON BreakageEnd where
  toJSON = genericToJSON JsonUtils.dropUnderscore

type BreakageEndRecord = DbHelpers.WithId (DbHelpers.WithAuthorship BreakageEnd)


data BreakageSpan = BreakageSpan {
    _start :: DbHelpers.WithId (DbHelpers.WithAuthorship BreakageStart)
  , _end   :: Maybe BreakageEndRecord
  } deriving Generic

instance ToJSON BreakageSpan where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data MasterBuildsResponse = MasterBuildsResponse {
    _columns        :: [Text]
  , _commits        :: [IndexedCommit]
  , _failures       :: [SimpleBuildStatus]
  , _breakage_spans :: [BreakageSpan]
  } deriving Generic

instance ToJSON MasterBuildsResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore
