{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module StatusEvent where

import           Data.Aeson
import           Data.Text.Lazy (Text)
import           GHC.Generics

import qualified JsonUtils


data GitHubStatusEventSetter = GitHubStatusEventSetter {
    _description :: Text
  , _state       :: Text
  , _target_url  :: Text
  , _context     :: Text
  } deriving (Generic, Show)

instance ToJSON GitHubStatusEventSetter where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromJSON GitHubStatusEventSetter where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore
