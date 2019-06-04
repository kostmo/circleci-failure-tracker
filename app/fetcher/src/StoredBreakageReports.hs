{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module StoredBreakageReports where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified AuthStages
import qualified Builds
import qualified JsonUtils


data BreakageReport = BreakageReport {
    _build               :: Builds.BuildNumber
  , _step_name           :: Text
  , _job_name            :: Text
  , _is_broken           :: Bool
  , _reporter            :: AuthStages.Username
  , _reported_at         :: UTCTime
  , _notes               :: Maybe Text
  , _implicated_revision :: Maybe Text
  } deriving Generic

instance ToJSON BreakageReport where
  toJSON = genericToJSON JsonUtils.dropUnderscore
