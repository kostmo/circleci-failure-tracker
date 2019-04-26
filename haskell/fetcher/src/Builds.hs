{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Builds where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics
import           GHC.Int      (Int64)


data BuildNumber = NewBuildNumber Int64
  deriving (Show, Generic)

data BuildStepId = NewBuildStepId Int64
  deriving (Show, Generic)



instance ToJSON BuildNumber
instance FromJSON BuildNumber


data Build = NewBuild {
    build_id     :: BuildNumber
  , vcs_revision :: Text
  , queued_at    :: UTCTime
  , job_name     :: Text
  } deriving (Show, Generic)

instance ToJSON Build
instance FromJSON Build


data BuildStepFailure = NewBuildStepFailure {
    step_name    :: Text
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
