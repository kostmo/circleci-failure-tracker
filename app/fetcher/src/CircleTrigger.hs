{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CircleTrigger where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Data.Aeson
import qualified Data.ByteString            as BS (empty)
import           Data.List                  (intercalate)
import           Data.Text                  (Text)
import           GHC.Generics
import qualified Network.Wreq               as NW

import qualified Builds
import qualified CircleApi
import qualified Constants
import qualified FetchHelpers


data CircleBuildRetryResponse = CircleBuildRetryResponse {
    build_num    :: Builds.BuildNumber
  , branch       :: Text
  , vcs_revision :: Builds.RawCommit
  , retry_of     :: Builds.BuildNumber
  } deriving (Show, Generic)

instance FromJSON CircleBuildRetryResponse


-- | Note: This does not work when a job has a
-- "Attaching Workspace" step that requires a Workflow.
-- The message in such case will be:
--
--   Warning: skipping this step: Missing workflow workspace identifiers,
--   this step must be run in the context of a workflow
--
-- Subsequent steps then fail.
-- Instead, use the CircleCI 2.0 API that allows
-- triggering jobs within a workflow.
rebuildCircleJobStandalone ::
     CircleApi.CircleCIApiToken
  -> Builds.BuildNumber
  -> ExceptT String IO CircleBuildRetryResponse
rebuildCircleJobStandalone
    (CircleApi.CircleCIApiToken circleci_api_token)
    (Builds.NewBuildNumber build_num) = do

  parsed_api_response <- ExceptT $ FetchHelpers.safeGetUrl $ NW.postWith opts rebuild_url BS.empty
  r <- NW.asJSON parsed_api_response
  return $ r ^. NW.responseBody
  where
    opts = NW.defaults
      & NW.header "Accept" .~ [Constants.jsonMimeType]
      & NW.param "circle-token" .~ [circleci_api_token]

    rebuild_url = intercalate "/" [
        Constants.circleciApiBase
      , show build_num
      , "retry"
      ]



-- Good test API:
-- BUILD_NUM=4345109; curl -s https://circleci.com/api/v1.1/project/github/pytorch/pytorch/$BUILD_NUM/tests

-- Bad test API:
-- BUILD_NUM=4345109; curl -s -X GET https://circleci.com/api/v2/project/github/pytorch/pytorch/$BUILD_NUM/tests?circle-token=XXXXX -H 'Accept: application/json'


data RebuildPayload = RebuildPayload {
    jobs :: [Text]
  } deriving Generic

instance ToJSON RebuildPayload


rebuildCircleJobInWorkflow ::
     CircleApi.CircleCIApiToken
  -> Builds.BuildNumber
  -> ExceptT String IO CircleBuildRetryResponse
rebuildCircleJobInWorkflow
    (CircleApi.CircleCIApiToken circleci_api_token)
    (Builds.NewBuildNumber _build_num) = do

  parsed_api_response <- ExceptT $ FetchHelpers.safeGetUrl $
    NW.postWith opts rebuild_url $ toJSON $ RebuildPayload job_ids

  r <- NW.asJSON parsed_api_response
  return $ r ^. NW.responseBody
  where
    opts = NW.defaults
      & NW.header "Accept" .~ [Constants.jsonMimeType]
      & NW.header "'Content-Type" .~ [Constants.jsonMimeType]
      & NW.param "circle-token" .~ [circleci_api_token]

    job_ids = ["08932615-abdb-4813-acf9-1dce99da522b"]

    workflow_id = "1716d89a-6dd4-4fe0-94b4-eb4360bfdd3a"
    rebuild_url = intercalate "/" [
        "https://circleci.com/api/v2/workflow"
      , workflow_id
      , "rerun"
      ]
