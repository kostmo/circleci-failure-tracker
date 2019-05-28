{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Webhooks where

import           Data.Aeson
import           Data.Text.Lazy   (Text)
import           Data.Time        (UTCTime)
import           GHC.Generics

import qualified JsonUtils
import qualified StatusEventQuery


data GitHubStatusEvent = GitHubStatusEvent {
    sha         :: Text
  , name        :: Text -- ^ <org>/<repo>
  , description :: Text
  , state       :: Text
  , target_url  :: Text
  , context     :: Text
  , created_at  :: UTCTime
  , updated_at  :: UTCTime
  } deriving Generic

instance FromJSON GitHubStatusEvent


data GitHubCombinedStatuses = GitHubCombinedStatuses {
--    _state    :: Text -- ^ collides with GitHubStatusEventSetter
    _statuses    :: [StatusEventQuery.GitHubStatusEventGetter]
  , _total_count :: Int
  } deriving (Generic, Show)

instance FromJSON GitHubCombinedStatuses where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore
