{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BreakageReportsBackup where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified AuthStages
import qualified Builds
import qualified JsonUtils


data DbBreakageReport = DbBreakageReport {
    _reporter            :: AuthStages.Username
  , _reported_at         :: UTCTime
  , _step_id             :: Builds.BuildStepId
  , _is_broken           :: Bool
  , _implicated_revision :: Maybe Text
  , _notes               :: Maybe Text
  } deriving Generic

instance ToJSON DbBreakageReport where
  toJSON = genericToJSON JsonUtils.dropUnderscore
