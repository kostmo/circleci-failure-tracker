{-# LANGUAGE DeriveGeneric #-}

module CommitBuilds where

import           Data.Aeson
import           GHC.Generics

import qualified Builds
import qualified DbHelpers
import qualified JsonUtils
import qualified MatchOccurrences


data CommitBuild = NewCommitBuild {
    _build    :: Builds.StorableBuild
  , _match    :: MatchOccurrences.MatchOccurrencesForBuild
  , _provider :: DbHelpers.WithId Builds.CiProvider
  } deriving Generic

instance ToJSON CommitBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore
