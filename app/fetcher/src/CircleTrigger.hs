{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CircleTrigger where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except)
import           Data.Aeson
import qualified Data.ByteString            as BS (empty)
import           Data.Either.Utils          (maybeToEither)
import           Data.List                  (intercalate)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import qualified Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess

import qualified Builds
import qualified CircleApi
import qualified CircleBuild
import qualified Constants
import qualified FetchHelpers


data CircleBuildRetryResponse = CircleBuildRetryResponse {
    build_num    :: Builds.BuildNumber
  , branch       :: Text
  , vcs_revision :: Builds.RawCommit
  , retry_of     :: Builds.BuildNumber
  } deriving (Show, Generic)

instance FromJSON CircleBuildRetryResponse


data CircleV2BuildRetryResponse = CircleV2BuildRetryResponse {
    message    ::  Text
  } deriving (Show, Generic)

instance FromJSON CircleV2BuildRetryResponse


-- | Note: This does not work when a job has a
-- "Attaching Workspace" step that requires a Workflow.
-- The message in such case will be:
--
--   Warning: skipping this step: Missing workflow workspace identifiers,
--   this step must be run in the context of a workflow
--
-- Subsequent steps then fail.
-- This was confirmed in a support thread:
--  https://support.circleci.com/hc/en-us/requests/66473?flash_digest=3617af28162f254ac2685d64d34f22f984939309&flash_digest=904946ebf8b4f6a186709174af398bdd86cc9f81&page=1#_=_
--
-- Instead, use the CircleCI 2.0 API that allows
-- triggering jobs within a workflow.
--
-- There is still some unsolved mystery about this, though.
-- The pytorchbot "retest this please" uses the v1.X
-- CircleCI API to rerun jobs, without suffering from
-- this type of "Attaching Workspace (skipped)" failure:
--   https://our.internmc.facebook.com/intern/paste/P125625464/
rebuildCircleJobStandalone ::
     CircleApi.CircleCIApiToken
  -> Builds.BuildNumber
  -> ExceptT String IO CircleBuildRetryResponse
rebuildCircleJobStandalone
    (CircleApi.CircleCIApiToken circleci_api_token)
    (Builds.NewBuildNumber build_num) = do

  api_response <- ExceptT $ FetchHelpers.safeGetUrl $
    NW.postWith opts rebuild_url BS.empty

  r <- NW.asJSON api_response
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


{- HLINT ignore "Use newtype instead of data" -}
data RebuildPayload = RebuildPayload {
    jobs :: [Text]
  } deriving Generic

instance ToJSON RebuildPayload


rebuildCircleJobInWorkflow ::
     CircleApi.CircleCIApiToken
  -> Builds.BuildNumber
  -> ExceptT String IO CircleV2BuildRetryResponse
rebuildCircleJobInWorkflow
    tok@(CircleApi.CircleCIApiToken circleci_api_token)
    provider_build_num = do

  circle_sess <- liftIO Sess.newSession
  single_build <- ExceptT $ CircleBuild.getCircleCIBuildPayload tok circle_sess provider_build_num

  workflow_obj <- except $ maybeToEither "CircleCI build record has no workflow data!" $ CircleBuild.workflows single_build

  let rebuild_url = intercalate "/" [
          "https://circleci.com/api/v2/workflow"
        , T.unpack $ CircleBuild.workflow_id workflow_obj
        , "rerun"
        ]

      job_ids = [CircleBuild.job_id workflow_obj]

  api_response <- ExceptT $ FetchHelpers.safeGetUrl $
    NW.postWith opts rebuild_url $ toJSON $ RebuildPayload job_ids

  r <- NW.asJSON api_response
  return $ r ^. NW.responseBody
  where

    opts = NW.defaults
      & NW.header "Accept" .~ [Constants.jsonMimeType]
      & NW.header "'Content-Type" .~ [Constants.jsonMimeType]
      & NW.param "circle-token" .~ [circleci_api_token]



