{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CommitBuilds where

import           Data.Aeson
import           GHC.Generics

import qualified Builds
import qualified MatchOccurrences


data CommitBuild = NewCommitBuild {
    build :: Builds.Build
  , match :: MatchOccurrences.MatchOccurrencesForBuild
  } deriving Generic

instance ToJSON CommitBuild



