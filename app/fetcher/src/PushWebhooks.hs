{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PushWebhooks where

import           Data.Aeson
import           Data.Text.Lazy (Text)
--import           Data.Time      (UTCTime)
import           GHC.Generics


-- | See API documentation:
-- https://developer.github.com/v3/activity/events/types/#pushevent
data GitHubPushEvent = GitHubPushEvent {
    ref         :: Text
  , head_commit :: HeadCommit -- ^ XXX The documentation is *wrong*; the key name is "head_commit" not "head"
  , before      :: Text
  } deriving Generic

instance FromJSON GitHubPushEvent


data HeadCommit = HeadCommit {
    id      :: Text
  , tree_id :: Text
  } deriving Generic

instance FromJSON HeadCommit




