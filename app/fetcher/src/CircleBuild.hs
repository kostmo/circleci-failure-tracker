{-# LANGUAGE DeriveGeneric #-}

module CircleBuild where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified Builds
import qualified JsonUtils


data SingleBuild = SingleBuild {
    vcs_revision     :: Text
  , queued_at        :: UTCTime
  , branch           :: Text
  , build_parameters :: BuildParametersChild
  , workflows        :: Maybe WorkflowChild
    -- ^ this field is not always present; in particular, it has been absent
    -- when "retry_of" is non-null
  , steps            :: [Value]
  , failed           :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON SingleBuild


data WorkflowChild = WorkflowChild {
    job_name     :: Text
  } deriving (Show, Generic)

instance FromJSON WorkflowChild


data BuildParametersChild = BuildParametersChild {
    _CIRCLE_JOB     :: Text
  } deriving (Show, Generic)

instance FromJSON BuildParametersChild where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore


toBuild :: Builds.BuildNumber -> SingleBuild -> Builds.Build
toBuild build_num single_build = Builds.NewBuild
  build_num
  (Builds.RawCommit $ vcs_revision single_build)
  (queued_at single_build)
  jobname
  (branch single_build)
  where
    jobname = maybe (_CIRCLE_JOB $ build_parameters single_build) job_name $ workflows single_build

