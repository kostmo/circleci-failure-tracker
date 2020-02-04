{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CircleTest where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
import           Data.Aeson
import           Data.List                  (intercalate)
import           Data.Text                  (Text)
import           GHC.Generics
import qualified Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess

import qualified Builds
import qualified CircleApi
import qualified Constants
import qualified FetchHelpers
import qualified ScanRecords


-- Good test API:
-- BUILD_NUM=4345109; curl -s https://circleci.com/api/v1.1/project/github/pytorch/pytorch/$BUILD_NUM/tests

-- Bad test API:
-- BUILD_NUM=4345109; curl -s -X GET https://circleci.com/api/v2/project/github/pytorch/pytorch/$BUILD_NUM/tests?circle-token=XXXXX -H 'Accept: application/json'


{- HLINT ignore "Use newtype instead of data" -}
data CircleCISingleTestsParent = CircleCISingleTestsParent {
    tests :: [CircleCISingleTestResult]
  } deriving Generic

instance FromJSON CircleCISingleTestsParent


-- | Note: The "message" field is non-null when the value of
-- the "result" field is "failure"
data CircleCISingleTestResult = CircleCISingleTestResult {
    classname   :: Text
  , file        :: Text
  , name        :: Text
  , result      :: Text
  , run_time    :: Double
  , message     :: Maybe Text
  , source      :: Text
  , source_type :: Text
  } deriving (Show, Generic)

instance FromJSON CircleCISingleTestResult


getCircleTestResults ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> ExceptT String IO CircleCISingleTestsParent
getCircleTestResults
    scan_resources
    (Builds.NewBuildNumber provider_build_num) = do

  api_response <- ExceptT $ FetchHelpers.safeGetUrl $
    Sess.getWith opts sess test_results_url

  r <- NW.asJSON api_response
  return $ r ^. NW.responseBody
  where
    fetching_resources = ScanRecords.fetching scan_resources
    CircleApi.CircleCIApiToken circleci_api_token = ScanRecords.circle_token fetching_resources
    sess = ScanRecords.circle_sess fetching_resources
    test_results_url = intercalate "/" [
        Constants.circleciApiBase
      , show provider_build_num
      , "tests"
      ]

    opts = NW.defaults
      & NW.header "Accept" .~ [Constants.jsonMimeType]
      & NW.header "'Content-Type" .~ [Constants.jsonMimeType]
      & NW.param "circle-token" .~ [circleci_api_token]
