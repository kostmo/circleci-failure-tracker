{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CommitBuilds where

import           Data.Aeson
import           Data.Time        (UTCTime)
import           GHC.Generics

import qualified AuthStages
import qualified Builds
import qualified JsonUtils
import qualified MatchOccurrences


data StoredBreakageReport = StoredBreakageReport {
    _is_broken   :: Bool
  , _reporter    :: AuthStages.Username
  , _reported_at :: UTCTime
  } deriving Generic

instance ToJSON StoredBreakageReport where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data CommitBuild = NewCommitBuild {
    _build    :: Builds.Build
  , _match    :: MatchOccurrences.MatchOccurrencesForBuild
  , _breakage :: Maybe StoredBreakageReport
  } deriving Generic

instance ToJSON CommitBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore
