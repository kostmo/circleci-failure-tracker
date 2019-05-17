{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Breakages where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified AuthStages


data BreakageReport = NewBreakageReport {
    revision            :: Text
  , implicated_revision :: Maybe Text
  , is_broken           :: Bool
  , notes               :: Text
  , reporter            :: AuthStages.Username
  } deriving Generic

instance ToJSON BreakageReport
instance FromJSON BreakageReport


