{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHubRecords where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified JsonUtils


data GitHubCommit = GitHubCommit {
    _sha     :: Text
  , _parents :: Maybe [ParentCommit]
  , _commit  :: CommitPart
  } deriving Generic

instance ToJSON GitHubCommit where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromJSON GitHubCommit where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

extractCommitSha :: GitHubCommit -> Text
extractCommitSha (GitHubCommit x _ _) = x


data ParentCommit = ParentCommit {
    _sha :: Text
  , _url :: Text
  } deriving Generic

instance ToJSON ParentCommit where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromJSON ParentCommit where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

extractParentSha :: ParentCommit -> Text
extractParentSha (ParentCommit x _) = x


data AuthorPart = AuthorPart {
    _name  :: Text
  , _email :: Text
  , _date  :: UTCTime
  } deriving Generic

instance FromJSON AuthorPart where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

instance ToJSON AuthorPart where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data CommitPart = CommitPart {
    _author    :: AuthorPart
  , _committer :: AuthorPart
  , _message   :: Text
  , _tree      :: TreePart
  } deriving Generic

instance FromJSON CommitPart where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

instance ToJSON CommitPart where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data TreePart = TreePart {
    _sha :: Text
  , _url :: Text
  } deriving Generic

instance FromJSON TreePart where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore

instance ToJSON TreePart where
  toJSON = genericToJSON JsonUtils.dropUnderscore

