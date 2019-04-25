{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scanning where

import Network.Wreq
import qualified Network.Wreq.Session as Sess

import Control.Lens hiding ((<.>))
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import Data.Maybe (Maybe)
import Data.Aeson.Lens (_String, _Array, _Bool, _Integral, key)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Aeson (Value, decode, encode)
import qualified Data.Vector as V
import System.FilePath
import Control.Monad (unless)
import Data.Traversable (for)

import Builds
import SillyMonoids ()
import qualified Constants
import qualified LogCache
import qualified Helpers
import qualified SqlWrite
import qualified ScanPatterns


maxBuildPerPage = 100


itemToBuild :: Value -> Build
itemToBuild json = NewBuild {
    build_id = NewBuildNumber $ view (key "build_num" . _Integral) json
  , vcs_revision = view (key "vcs_revision" . _String) json
  , queued_at = head $ Maybe.fromJust $ decode (encode [queued_at_string]) 
  , job_name = view (key "workflows" . key "job_name" . _String) json
  }
  where
    queued_at_string = view (key "queued_at" . _String) json


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


get_all_failed_build_info :: [Build] -> IO [Either (Build, BuildStepFailure) ()]
get_all_failed_build_info builds_list = do

  sess <- Sess.newSession
  for builds_list (Scanning.get_failed_build_info sess)


get_failed_build_info :: Sess.Session -> Build -> IO (Either (Build, BuildStepFailure) ())
get_failed_build_info sess build_object = do

  putStrLn $ "Fetching from: " ++ fetch_url

  r <- Sess.getWith opts sess fetch_url
  let steps_list = r ^. responseBody . key "steps" . _Array
      either_failed_step = mapM_ get_build_failure steps_list

  return $ Helpers.transformLeft ((build_object, )) either_failed_step

  where
    build_number = build_id build_object
    fetch_url = get_single_build_url build_number
    opts = defaults & header "Accept" .~ [Constants.json_mime_type]


get_single_build_list :: Sess.Session -> Int -> Int -> IO [Build]
get_single_build_list sess limit offset = do

  r <- Sess.getWith opts sess fetch_url
  let inner_list = r ^. responseBody . _Array
      builds_list = map itemToBuild $ V.toList inner_list

  return builds_list

  where
    fetch_url = get_build_list_url "master"
    opts = defaults
      & header "Accept" .~ [Constants.json_mime_type]
      & param "shallow" .~ ["true"]
      & param "filter" .~ ["failed"]
      & param "offset" .~ [T.pack $ show offset]
      & param "limit" .~ [T.pack $ show limit]


populate_builds :: Int -> IO [Build]
populate_builds max_build_count = do

  sess <- Sess.newSession
  get_single_build_list sess builds_per_page 0

  where
    builds_per_page = min maxBuildPerPage max_build_count


filter_scannable :: (Build, BuildStepFailure) -> Maybe (BuildNumber, BuildFailureOutput)
filter_scannable (z, a_build) = case failure_mode a_build of
  BuildTimeoutFailure -> Nothing
  ScannableFailure x -> Just (build_id z, x)


store_all_logs :: [(BuildNumber, BuildFailureOutput)] -> IO ()
store_all_logs scannable = do

  createDirectoryIfMissing True Constants.url_cache_basedir

--  pages <- withTaskGroup 4 $ \g -> mapConcurrently g Scanning.store_log scannable
--  pages <- mapConcurrently Scanning.store_log scannable

--  pages <- withPool 1 $ \pool -> parallel_ pool $ map Scanning.store_log scannable

  sess <- Sess.newSession

  pages <- mapM_ (Scanning.store_log sess) scannable
  return ()


gen_cached_path_prefix :: BuildNumber -> String
gen_cached_path_prefix (NewBuildNumber build_num) =
  Constants.url_cache_basedir </> filename_stem
  where
    filename_stem = show build_num


-- | Not used yet; this stuff should be in the database.
gen_metadata_path :: BuildNumber -> String
gen_metadata_path build_number = gen_cached_path_prefix build_number <.> "meta"


gen_log_path :: BuildNumber -> String
gen_log_path build_number = gen_cached_path_prefix build_number <.> "log"


store_log :: Sess.Session -> (BuildNumber, BuildFailureOutput) -> IO ()
store_log sess (build_number, failed_build_output) = do

  is_file_existing <- doesFileExist full_filepath

  putStrLn $ "Does log exist at path " ++ full_filepath ++ "? " ++ show is_file_existing

  unless is_file_existing $ do

      putStrLn $ "Log not on disk. Downloading from: " ++ T.unpack download_url
      r <- Sess.get sess $ T.unpack download_url
      let parent_elements = r ^. responseBody . _Array
          console_log = (V.head parent_elements) ^. key "message" . _String

      TIO.writeFile full_filepath console_log

  where
    download_url = log_url failed_build_output
    full_filepath = gen_log_path build_number


scan_logs :: [ScanPatterns.Pattern] -> BuildNumber -> IO [[ScanMatch]]
scan_logs patterns build_number = do

  console_log <- TIO.readFile full_filepath
  return $ filter (not . null) $ map apply_patterns $ map T.stripEnd $ T.lines console_log

  where
    apply_patterns line = Maybe.mapMaybe (apply_single_pattern line) patterns

    apply_single_pattern line pattern_obj = if ScanPatterns.is_regex pattern_obj
      then Nothing -- FIXME
      else if T.isInfixOf pattern_text line
        then Just $ NewScanMatch pattern_text line
        else Nothing
      where
        pattern_text = ScanPatterns.expression pattern_obj

    full_filepath = gen_log_path build_number


