{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CircleBuild where

import           Data.Aeson
import           Data.Aeson   (Value)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified Builds


data SingleBuild = SingleBuild {
    vcs_revision :: Text
  , queued_at    :: UTCTime
  , branch       :: Text
  , workflows    :: WorkflowChild
  , steps        :: [Value]
  , failed       :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON SingleBuild
instance FromJSON SingleBuild


data WorkflowChild = WorkflowChild {
    job_name     :: Text
  } deriving (Show, Generic)

instance ToJSON WorkflowChild
instance FromJSON WorkflowChild


toBuild :: Builds.BuildNumber -> SingleBuild -> Builds.Build
toBuild build_num single_build = Builds.NewBuild
  build_num
  (vcs_revision single_build)
  (queued_at single_build)
  (job_name $ workflows single_build)
  (branch single_build)
