{-# LANGUAGE DeriveGeneric #-}

module GitHubRecords where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified JsonUtils


data GitHubCommit = GitHubCommit {
    _sha     :: Text
  , _parents :: Maybe [GitHubCommit]
  } deriving Generic

instance ToJSON GitHubCommit where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromJSON GitHubCommit where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore
