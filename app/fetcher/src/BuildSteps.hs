{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildSteps where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified Breakages
import qualified Builds

data BuildStep = NewBuildStep {
    step_name :: Text
  , step_id   :: Builds.BuildStepId
  , build     :: Builds.Build
  , breakage  :: Maybe Breakages.BreakageReport
  } deriving Generic

instance ToJSON BuildStep
instance FromJSON BuildStep


