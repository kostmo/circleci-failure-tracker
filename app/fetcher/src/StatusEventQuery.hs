{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module StatusEventQuery where

import           Data.Aeson
import           Data.Text.Lazy (Text)
import           Data.Time      (UTCTime)
import           GHC.Generics

import qualified JsonUtils

-- | This is just like Webhooks.GitHubStatusEvent
-- except without the @sha@ and @name@ fields.
data GitHubStatusEventGetter = GitHubStatusEventGetter {
    _url         :: Text
  , _avatar_url  :: Text
  , _id          :: Integer
  , _state       :: Text
  , _description :: Text
  , _target_url  :: Text
  , _context     :: Text
  , _created_at  :: UTCTime
  } deriving (Generic, Show)

instance ToJSON GitHubStatusEventGetter where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromJSON GitHubStatusEventGetter where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore
