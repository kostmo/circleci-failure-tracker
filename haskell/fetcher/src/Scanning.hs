{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scanning where

import Network.Wreq
import Control.Lens
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import Data.Maybe (Maybe)
import Data.Aeson.Lens (_String, _Array, _Bool, _Integral, key)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (Value)
import qualified Data.Vector as V
import Data.Time.Format (parseTimeOrError, defaultTimeLocale, rfc822DateFormat)
import System.FilePath

import SillyMonoids ()
import qualified Constants
import qualified LogCache
import Builds


itemToBuild :: Value -> Build
itemToBuild json = NewBuild {
    build_id = NewBuildNumber $ view (key "build_num" . _Integral) json
  , vcs_revision = view (key "vcs_revision" . _String) json
  , queued_at = parseTimeOrError False defaultTimeLocale rfc822DateFormat $
      T.unpack $ view (key "queued_at" . _String) json
  , job_name = view (key "workflows" . key "job_name" . _String) json
  }


get_single_build_url :: BuildNumber -> String
get_single_build_url (NewBuildNumber build_number) = intercalate "/"
  [ Constants.circleci_api_base
  , show build_number
  ]


get_build_list_url branch_name = intercalate "/"
  [ Constants.circleci_api_base
  , "tree"
  , branch_name
  ]


get_build_failure :: Value -> Either BuildStepFailure ()
get_build_failure step_val =
  mapM_ get_failure my_array
  where
    my_array = step_val ^. key "actions" . _Array
    step_name = step_val ^. key "name" . _String

    step_fail = Left . NewBuildStepFailure step_name

    get_failure x
      | (x ^. key "failed" . _Bool) = step_fail $
          ScannableFailure $ NewBuildFailureOutput $ x ^. key "output_url" . _String
      | (x ^. key "timedout" . _Bool) = step_fail BuildTimeoutFailure
      | otherwise = pure ()




transformLeft :: (a -> c) -> Either a b -> Either c b
transformLeft transformer original = case original of
  Left x -> Left $ transformer x
  Right x -> Right x



get_failed_build_info :: Build -> IO (Either (Build, BuildStepFailure) ())
get_failed_build_info build_object = do

  putStrLn $ "Fetching from: " ++ fetch_url

  r <- getWith opts fetch_url
  let steps_list = r ^. responseBody . key "steps" . _Array
      either_failed_step = mapM_ get_build_failure steps_list

  return $ transformLeft ((build_object, )) either_failed_step

  where
    build_number = build_id build_object
    fetch_url = get_single_build_url build_number
    opts = defaults & header "Accept" .~ [Constants.json_mime_type]


populate_builds :: Int -> Int -> IO [Build]
populate_builds limit offset = do

  r <- getWith opts fetch_url
  let inner_list = r ^. responseBody . _Array
      builds_list = map itemToBuild $ V.toList inner_list

  return builds_list

  where
    fetch_url = get_build_list_url "master"
    opts = defaults
      & header "Accept" .~ [Constants.json_mime_type]
      & param "offset" .~ [T.pack $ show offset]
      & param "shallow" .~ ["true"]
      & param "limit" .~ [T.pack $ show limit]
      & param "filter" .~ ["failed"]


filter_scannable :: (Build, BuildStepFailure) -> Maybe (BuildNumber, BuildFailureOutput)
filter_scannable (z, a_build) = case failure_mode a_build of
  BuildTimeoutFailure -> Nothing
  ScannableFailure x -> Just (build_id z, x)




store_log :: (BuildNumber, BuildFailureOutput) -> IO ()
store_log (NewBuildNumber build_number, failed_build_output) = do

  r <- get $ T.unpack $ log_url failed_build_output
  let parent_elements = r ^. responseBody . _Array
      console_log = (V.head parent_elements) ^. key "message" . _String

  TIO.writeFile full_filepath console_log
  where
    filename = show build_number
    full_filepath = Constants.url_cache_basedir </> filename


{-
scan_logs :: [Text] -> BuildNumber -> IO [[ScanMatch]]
scan_logs patterns build_number = do

  return $ map apply_patterns $ map T.stripEnd $ T.lines console_log
  where
    apply_patterns line = Maybe.mapMaybe (apply_single_pattern line) patterns

    apply_single_pattern line pattern = if T.isInfixOf pattern line
      then Just $ NewScanMatch pattern line
      else Nothing
-}

