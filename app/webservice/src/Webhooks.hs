{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Webhooks where

import           Data.Aeson
import           Data.Text.Lazy (Text)
import           Data.Time      (UTCTime)
import           GHC.Generics

import qualified JsonUtils


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


data GitHubStatusEventSetter = GitHubStatusEventSetter {
    _description :: Text
  , _state       :: Text
  , _target_url  :: Text
  , _context     :: Text
  } deriving Generic

instance ToJSON GitHubStatusEventSetter where
  toJSON = genericToJSON JsonUtils.dropUnderscore
