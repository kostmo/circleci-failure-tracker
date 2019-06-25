{-# LANGUAGE DeriveGeneric #-}

module Builds where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics
import           GHC.Int      (Int64)


newtype RawCommit = RawCommit Text
 deriving (Generic, Show)

instance ToJSON RawCommit
instance FromJSON RawCommit

newtype BuildNumber = NewBuildNumber Int64
  deriving (Show, Generic, Eq, Ord)

instance ToJSON BuildNumber
instance FromJSON BuildNumber


newtype BuildStepId = NewBuildStepId Int64
  deriving (Show, Generic)

instance ToJSON BuildStepId
instance FromJSON BuildStepId


data Build = NewBuild {
    build_id     :: BuildNumber
  , vcs_revision :: RawCommit
  , queued_at    :: UTCTime
  , job_name     :: Text
  , branch       :: Text
  } deriving (Show, Generic)

instance ToJSON Build
instance FromJSON Build


data BuildWithStepFailure = BuildWithStepFailure {
    build_object        :: Build
  , step_failure_object :: BuildStepFailure
  } deriving Show


data BuildStepFailure = NewBuildStepFailure {
    step_name    :: Text
  , failure_mode :: BuildFailureMode
  } deriving Show


-- | There can be different modes in which the build fails.
data BuildFailureMode =
    BuildTimeoutFailure
  | ScannableFailure BuildFailureOutput
  deriving Show


newtype BuildFailureOutput = NewBuildFailureOutput {
    log_url :: Text
  } deriving Show
