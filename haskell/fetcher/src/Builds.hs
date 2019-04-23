{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Builds where

import           Data.Aeson
import           GHC.Generics
import Data.Text (Text)
import Data.Time (UTCTime)


data BuildNumber = NewBuildNumber Int
  deriving (Show, Generic)

instance ToJSON BuildNumber
instance FromJSON BuildNumber


data Build = NewBuild {
    build_id :: BuildNumber
  , vcs_revision :: Text
  , queued_at :: UTCTime
  , job_name :: Text
  } deriving (Show, Generic)

instance ToJSON Build
instance FromJSON Build



data SimpleBuild = NewSimpleBuild {
    build_num :: Int
  , build_job_name :: Text
  } deriving (Show, Generic)


instance ToJSON SimpleBuild
instance FromJSON SimpleBuild


data BuildStepFailure = NewBuildStepFailure {
    step_name :: Text
  , failure_mode :: BuildFailureMode
  } deriving Show


-- | There can be different modes in which the build fails.
data BuildFailureMode =
    BuildTimeoutFailure
  | ScannableFailure BuildFailureOutput
  deriving Show


data BuildFailureOutput = NewBuildFailureOutput {
    log_url :: Text
  } deriving Show


data ScanMatch = NewScanMatch {
    scanned_pattern :: Text
  , matching_line :: Text
  } deriving Show

