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
    ref  :: Text
  , head :: Text
--  , before :: Text
--  , size   :: Integer
  } deriving Generic

instance FromJSON GitHubPushEvent


