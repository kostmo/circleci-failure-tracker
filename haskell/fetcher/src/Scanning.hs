{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import           Control.Lens               hiding ((<.>))
import           Control.Monad              (unless)
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens            (key, _Array, _Bool, _String)
import           Data.Foldable              (for_)
import           Data.List                  (intercalate)
import           Data.Maybe                 (Maybe)
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Internal.Search  as Search
import qualified Data.Text.IO               as TIO
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess
import qualified Safe
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.FilePath
import           System.Posix.Files         (fileSize, getFileStatus)
import           System.Posix.Types         (COff (COff))
import           Text.Regex.Base
import           Text.Regex.PCRE            ((=~~))

import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified FetchHelpers
import qualified ScanPatterns
import qualified ScanRecords
import           SillyMonoids               ()
import qualified SqlRead
import qualified SqlWrite


get_single_build_url :: Builds.BuildNumber -> String
get_single_build_url (Builds.NewBuildNumber build_number) = intercalate "/"
  [ Constants.circleci_api_base
  , show build_number
  ]


get_step_failure :: Value -> Either Builds.BuildStepFailure ()
get_step_failure step_val =
  mapM_ get_failure my_array
  where
    my_array = step_val ^. key "actions" . _Array
    stepname = step_val ^. key "name" . _String

    step_fail = Left . Builds.NewBuildStepFailure stepname

    get_failure x
      | (x ^. key "failed" . _Bool) = step_fail $
          Builds.ScannableFailure $ Builds.NewBuildFailureOutput $ x ^. key "output_url" . _String
      | (x ^. key "timedout" . _Bool) = step_fail Builds.BuildTimeoutFailure
      | otherwise = pure ()


get_console_url :: (Builds.BuildNumber, Either Builds.BuildStepFailure ScanRecords.UnidentifiedBuildFailure) -> Maybe (Builds.BuildNumber, Builds.BuildFailureOutput)
get_console_url (build_number, maybe_thing) = case maybe_thing of
  Right _ -> Nothing
  Left (Builds.NewBuildStepFailure _stepname mode) -> case mode of
    Builds.BuildTimeoutFailure             -> Nothing
    Builds.ScannableFailure failure_output -> Just (build_number, failure_output)


prepare_scan_resources :: Connection -> IO ScanRecords.ScanCatchupResources
prepare_scan_resources conn = do

  aws_sess <- Sess.newSession
  circle_sess <- Sess.newSession

  cache_dir <- Constants.get_url_cache_basedir
  createDirectoryIfMissing True cache_dir

  pattern_records <- SqlRead.get_patterns conn
  let patterns_by_id = DbHelpers.to_dict pattern_records

  latest_pattern_id <- SqlRead.get_latest_pattern_id conn
  scan_id <- SqlWrite.insert_scan_id conn latest_pattern_id

  return $ ScanRecords.ScanCatchupResources
    conn
    aws_sess
    circle_sess
    scan_id
    latest_pattern_id
    patterns_by_id


catchup_scan :: ScanRecords.ScanCatchupResources -> Builds.BuildStepId -> (Builds.BuildNumber, Either Builds.BuildStepFailure ScanRecords.UnidentifiedBuildFailure) -> IO ()
catchup_scan scan_resources buildstep_id pair@(buildnum, visitation_result) = if build_has_log
  then do

    scannable_patterns <- SqlRead.get_unscanned_patterns_for_build scan_resources buildnum

    if null scannable_patterns
      then return ()

    else case maybe_console_url of
      Nothing        -> return ()
      Just console_url -> do
        store_log scan_resources console_url

        matches <- scan_log scan_resources buildstep_id buildnum scannable_patterns
        SqlWrite.store_matches scan_resources buildstep_id buildnum matches
        return ()
    return ()

  else
    return ()

  where
    build_has_log = case visitation_result of
      Right _ -> False
      Left (Builds.NewBuildStepFailure _ mode) -> case mode of
        Builds.BuildTimeoutFailure              -> False
        Builds.ScannableFailure _failure_output -> True

    maybe_console_url = get_console_url pair


-- | This function stores a record to the database
-- immediately upon build visitation. We do this instead of waiting
-- until the end so that we can resume progress if the process is
-- interrupted.
process_builds :: ScanRecords.ScanCatchupResources -> [Builds.BuildNumber] -> IO ()
process_builds scan_resources unvisited_builds_list = do

  for_ (zip [1::Int ..] unvisited_builds_list) $ \(idx, build_num) -> do
    putStrLn $ "Visiting " ++ show idx ++ "/" ++ show unvisited_count ++ " builds..."
    visitation_result <- get_failed_build_info scan_resources build_num

    let pair = (build_num, visitation_result)

    build_step_id <- SqlWrite.insert_build_visitation scan_resources pair

    catchup_scan scan_resources build_step_id pair
    return ()

  where
    unvisited_count = length unvisited_builds_list


-- | Determines which step of the build failed and stores
-- the console log to disk, if there is one.
--
-- Note that this function is a bit backwards in its use of Either;
-- here, the *expected* outcome is a Left, whereas a Right is the "bad" condition.
-- Rationale: we're searching a known-failed build for failures, so not finding a failure is unexpected.
-- We make use of Either's short-circuting to find the *first* failure.
get_failed_build_info :: ScanRecords.ScanCatchupResources -> Builds.BuildNumber -> IO (Either Builds.BuildStepFailure ScanRecords.UnidentifiedBuildFailure)
get_failed_build_info scan_resources build_number = do

  putStrLn $ "Fetching from: " ++ fetch_url

  either_r <- FetchHelpers.safeGetUrl $ Sess.getWith opts sess fetch_url

  return $ case either_r of
    Right r -> do
      let steps_list = r ^. NW.responseBody . key "steps" . _Array

      -- We expect to short circuit here and return a build step failure,
      -- but if we don't, we proceed
      -- to the NoFailedSteps return value.
      mapM_ get_step_failure steps_list
      return ScanRecords.NoFailedSteps

    Left err_message -> do
      let fail_string = "PROBLEM: Failed in get_failed_build_info with message: " ++ err_message
      return $ ScanRecords.NetworkProblem fail_string

  where
    fetch_url = get_single_build_url build_number
    opts = defaults & header "Accept" .~ [Constants.json_mime_type]
    sess = ScanRecords.circle_sess scan_resources


gen_log_path :: Builds.BuildNumber -> IO FilePath
gen_log_path (Builds.NewBuildNumber build_num) = do
  cache_dir <- Constants.get_url_cache_basedir
  return $ cache_dir </> filename_stem <.> "log"
  where
    filename_stem = show build_num


store_log :: ScanRecords.ScanCatchupResources -> (Builds.BuildNumber, Builds.BuildFailureOutput) -> IO ()
store_log scan_resources (build_number, failed_build_output) = do

  full_filepath <- gen_log_path build_number

  -- We normally shouldn't even need to perform this check, because upstream we've already
  -- filtered out pre-cached build logs via the SQL query.
  -- HOWEVER, the existence check at this layer is still useful for when the database is wiped (for development).
  is_file_existing <- doesFileExist full_filepath

  putStrLn $ "Does log exist at path " ++ full_filepath ++ "? " ++ show is_file_existing

  unless is_file_existing $ do

      putStrLn $ "Log not on disk. Downloading from: " ++ T.unpack download_url

      either_r <- FetchHelpers.safeGetUrl $ Sess.get aws_sess $ T.unpack download_url

      case either_r of
        Right r -> do
          let parent_elements = r ^. NW.responseBody . _Array
              -- we need to concatenate all of the "out" elements
              pred x = x ^. key "type" . _String == "out"
              output_elements = filter pred $ V.toList parent_elements

              console_log = mconcat $ map (\x -> x ^. key "message" . _String) output_elements

          TIO.writeFile full_filepath console_log
        Left err_message -> do
          putStrLn $ "PROBLEM: Failed in store_log with message: " ++ err_message
          return ()

  where
    download_url = Builds.log_url failed_build_output
    aws_sess = ScanRecords.aws_sess scan_resources


getFileSize :: String -> IO Int64
getFileSize path = do
    stat <- getFileStatus path
    let (COff bytecount) = fileSize stat
    return bytecount


apply_single_pattern (line_number, line) db_pattern = match_partial <$> match_span
  where
    match_span = case ScanPatterns.expression pattern_obj of
      ScanPatterns.RegularExpression regex_text -> case ((T.unpack line) =~~ regex_text :: Maybe (MatchOffset, MatchLength)) of
        Just (match_offset, match_length) -> Just $ ScanPatterns.NewMatchSpan match_offset (match_offset + match_length)
        Nothing -> Nothing
      ScanPatterns.LiteralExpression literal_text -> case Safe.headMay (Search.indices literal_text line) of
        Just first_index -> Just $ ScanPatterns.NewMatchSpan first_index (first_index + T.length literal_text)
        Nothing -> Nothing

    match_partial x = ScanPatterns.NewScanMatch db_pattern $ ScanPatterns.NewMatchDetails line line_number x
    pattern_obj = DbHelpers.record db_pattern


scan_log ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> Builds.BuildNumber
  -> [ScanPatterns.DbPattern]
  -> IO [ScanPatterns.ScanMatch]
scan_log scan_resources build_step_id build_number patterns = do

  full_filepath <- gen_log_path build_number
  putStrLn $ "Scanning log: " ++ full_filepath

  byte_count <- getFileSize full_filepath

  console_log <- TIO.readFile full_filepath
  let lines_list = T.lines console_log
      result = filter (not . null) $ map apply_patterns $ zip [0..] $ map T.stripEnd lines_list

  SqlWrite.store_log_info scan_resources build_step_id $ ScanRecords.LogInfo byte_count $ length lines_list

  return $ concat result

  where
    apply_patterns line_tuple = Maybe.mapMaybe (apply_single_pattern line_tuple) patterns
