{-# LANGUAGE DeriveGeneric #-}

module Commits where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified Builds
import qualified JsonUtils


data CommitMetadata = CommitMetadata {
    _sha1            :: Builds.RawCommit
  , _message         :: Text
  , _tree_sha1       :: Text
  , _author_name     :: Text
  , _author_email    :: Text
  , _author_date     :: UTCTime
  , _committer_name  :: Text
  , _committer_email :: Text
  , _committer_date  :: UTCTime
  } deriving Generic

instance FromJSON CommitMetadata where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

instance ToJSON CommitMetadata where
  toJSON = genericToJSON JsonUtils.dropUnderscore

