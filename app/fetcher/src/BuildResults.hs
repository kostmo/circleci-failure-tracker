{-# LANGUAGE DeriveGeneric #-}

module BuildResults where

import           Data.Aeson
import           Data.Text        (Text)
import           GHC.Generics


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


data MasterBuildsResponse = MasterBuildsResponse {
    _columns  :: [Text]
  , _commits  :: [IndexedCommit]
  , _failures :: [SimpleBuildStatus]
  } deriving Generic

instance ToJSON MasterBuildsResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore
