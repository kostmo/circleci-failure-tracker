{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Breakages where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified AuthStages
import qualified Builds


data BreakageReport = NewBreakageReport {
    build_step_id       :: Builds.BuildStepId
  , implicated_revision :: Maybe Text
  , is_broken           :: Bool
  , notes               :: Text
  , reporter            :: AuthStages.Username
  } deriving Generic

instance ToJSON BreakageReport
instance FromJSON BreakageReport


