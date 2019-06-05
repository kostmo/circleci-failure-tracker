{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens            (key, _Array, _Bool, _String)
import qualified Data.Either                as Either
import           Data.Either.Combinators    (rightToMaybe)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (intercalate)
import qualified Data.Maybe                 as Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Traversable           (for)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess
import qualified Safe

import qualified AuthStages
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified FetchHelpers
import qualified ScanPatterns
import qualified ScanRecords
import qualified ScanUtils
import           SillyMonoids               ()
import qualified SqlRead
import qualified SqlWrite


-- | Stores scan results to database and returns them.
scan_builds :: ScanRecords.ScanCatchupResources -> Either (Set Builds.BuildNumber) Int -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
scan_builds scan_resources whitelisted_builds_or_fetch_count = do

  visited_builds_list <- SqlRead.get_revisitable_builds conn
  let whitelisted_visited = visited_filter visited_builds_list
  rescan_matches <- rescan_visited_builds scan_resources whitelisted_visited

  unvisited_builds_list <- SqlRead.get_unvisited_build_ids conn maybe_fetch_limit
  let whitelisted_unvisited = unvisited_filter unvisited_builds_list
  first_scan_matches <- process_unvisited_builds scan_resources whitelisted_unvisited

  return $ rescan_matches ++ first_scan_matches

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
    maybe_fetch_limit = rightToMaybe whitelisted_builds_or_fetch_count

    (visited_filter, unvisited_filter) = case whitelisted_builds_or_fetch_count of
      Right _ -> (id, id)
      Left whitelisted_builds -> (
          filter $ (\(_, _, buildnum, _) -> buildnum `Set.member` whitelisted_builds)
        , filter (`Set.member` whitelisted_builds)
        )


rescan_single_build ::
     DbHelpers.DbConnectionData
  -> AuthStages.Username
  -> Builds.BuildNumber
  -> IO ()
rescan_single_build db_connection_data initiator build_to_scan = do
  putStrLn $ "Rescanning build: " ++ show build_to_scan
  conn <- DbHelpers.get_connection db_connection_data
  scan_resources <- prepare_scan_resources conn $ Just initiator
  scan_matches <- scan_builds scan_resources $ Left $ Set.singleton build_to_scan
  putStrLn $ "Found " ++ show (length scan_matches) ++ " matches."

  return ()


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


prepare_scan_resources :: Connection -> Maybe AuthStages.Username -> IO ScanRecords.ScanCatchupResources
prepare_scan_resources conn maybe_initiator = do

  aws_sess <- Sess.newSession
  circle_sess <- Sess.newSession

  pattern_records <- SqlRead.get_patterns conn
  let patterns_by_id = DbHelpers.to_dict pattern_records

  latest_pattern_id <- SqlRead.get_latest_pattern_id conn
  scan_id <- SqlWrite.insert_scan_id conn maybe_initiator latest_pattern_id

  return $ ScanRecords.ScanCatchupResources
    scan_id
    latest_pattern_id
    patterns_by_id $ ScanRecords.FetchingResources
      conn
      aws_sess
      circle_sess


get_pattern_objects :: ScanRecords.ScanCatchupResources -> [Int64] -> [ScanPatterns.DbPattern]
get_pattern_objects scan_resources =
  Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x (ScanRecords.patterns_by_id scan_resources))


-- | This only scans patterns if they are applicable to the particular
-- failed step of this build.
-- Patterns that are not annotated with applicability will apply
-- to any step.
catchup_scan :: ScanRecords.ScanCatchupResources -> Builds.BuildStepId -> T.Text -> (Builds.BuildNumber, Maybe Builds.BuildFailureOutput) -> [ScanPatterns.DbPattern] -> IO (Either String [ScanPatterns.ScanMatch])
catchup_scan scan_resources buildstep_id step_name (buildnum, maybe_console_output_url) scannable_patterns = do

  putStrLn $ "\tThere are " ++ (show $ length scannable_patterns) ++ " scannable patterns"

  let is_pattern_applicable p = null appl_steps || elem step_name appl_steps
        where
          appl_steps = ScanPatterns.applicable_steps $ DbHelpers.record p
      applicable_patterns = filter is_pattern_applicable scannable_patterns

  putStrLn $ "\t\twith " ++ (show $ length applicable_patterns) ++ " applicable to this step"

  -- | We only access the console log if there is at least one
  -- pattern to scan:
  case Safe.maximumMay $ map DbHelpers.db_id applicable_patterns of
    Nothing -> return $ Right []
    Just maximum_pattern_id -> runExceptT $ do

      lines_list <- ExceptT $ get_and_store_log scan_resources buildnum buildstep_id maybe_console_output_url
      let matches = scan_log_text lines_list applicable_patterns

      liftIO $ do
        SqlWrite.store_matches scan_resources buildstep_id buildnum matches
        SqlWrite.insert_latest_pattern_build_scan scan_resources buildnum maximum_pattern_id

      return matches


rescan_visited_builds :: ScanRecords.ScanCatchupResources -> [(Builds.BuildStepId, T.Text, Builds.BuildNumber, [Int64])] -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
rescan_visited_builds scan_resources visited_builds_list = do

  match_lists <- for (zip [1::Int ..] visited_builds_list) $ \(idx, (build_step_id, step_name, build_num, pattern_ids)) -> do
    putStrLn $ "Visiting " ++ show idx ++ "/" ++ show visited_count ++ " previously-visited builds (" ++ show build_num ++ ")..."

    either_matches <- catchup_scan scan_resources build_step_id step_name (build_num, Nothing) $
      get_pattern_objects scan_resources pattern_ids

    return $ (build_num, Either.fromRight [] either_matches)

  return match_lists

  where
    visited_count = length visited_builds_list


-- | This function stores a record to the database
-- immediately upon build visitation. We do this instead of waiting
-- until the end so that we can resume progress if the process is
-- interrupted.
process_unvisited_builds :: ScanRecords.ScanCatchupResources -> [Builds.BuildNumber] -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
process_unvisited_builds scan_resources unvisited_builds_list = do

  for (zip [1::Int ..] unvisited_builds_list) $ \(idx, build_num) -> do
    putStrLn $ "Visiting " ++ show idx ++ "/" ++ show unvisited_count ++ " unvisited builds..."
    visitation_result <- get_failed_build_info scan_resources build_num

    let pair = (build_num, visitation_result)
    build_step_id <- SqlWrite.insert_build_visitation scan_resources pair

    either_matches <- case visitation_result of
      Right _ -> return $ Right []
      Left (Builds.NewBuildStepFailure step_name mode) -> case mode of
        Builds.BuildTimeoutFailure             -> return $ Right []
        Builds.ScannableFailure failure_output ->
          catchup_scan scan_resources build_step_id step_name (build_num, Just failure_output) $
            ScanRecords.get_patterns_with_id scan_resources

    return $ (build_num, Either.fromRight [] either_matches)

  where
    unvisited_count = length unvisited_builds_list


-- | Determines which step of the build failed and stores
-- the console log to disk, if there is one.
--
-- Note that this function is a bit backwards in its use of Either;
-- here, the *expected* outcome is a Left, whereas a Right is the "bad" condition.
-- Rationale: we're searching a known-failed build for failures, so not finding a failure is unexpected.
-- We make use of Either's short-circuting to find the *first* failure.
get_failed_build_info ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> IO (Either Builds.BuildStepFailure ScanRecords.UnidentifiedBuildFailure)
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
    sess = ScanRecords.circle_sess $ ScanRecords.fetching scan_resources


get_and_store_log ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> Builds.BuildStepId
  -> Maybe Builds.BuildFailureOutput
  -> IO (Either String [T.Text])
get_and_store_log scan_resources build_number build_step_id maybe_failed_build_output = do

  maybe_console_log <- SqlRead.read_log conn build_number
  case maybe_console_log of
    Just console_log -> return $ Right $ T.lines console_log  -- Log was already fetched
    Nothing -> runExceptT $ do
      download_url <- ExceptT $ case maybe_failed_build_output of
        Just failed_build_output -> return $ Right $ Builds.log_url failed_build_output
        Nothing -> do
          visitation_result <- get_failed_build_info scan_resources build_number

          return $ case visitation_result of
            Right _ -> Left "This build didn't have a console log!"
            Left (Builds.NewBuildStepFailure _step_name mode) -> case mode of
              Builds.BuildTimeoutFailure             -> Left "This build didn't have a console log because it was a timeout!"
              Builds.ScannableFailure failure_output -> Right $ Builds.log_url failure_output


      liftIO $ putStrLn $ "Downloading log from: " ++ T.unpack download_url

      log_download_result <- ExceptT $ FetchHelpers.safeGetUrl $ Sess.get aws_sess $ T.unpack download_url

      let parent_elements = log_download_result ^. NW.responseBody . _Array
          -- we need to concatenate all of the "out" elements
          pred x = x ^. key "type" . _String == "out"
          output_elements = filter pred $ V.toList parent_elements

          console_log = mconcat $ map (\x -> x ^. key "message" . _String) output_elements
          lines_list = T.lines console_log
          byte_count = T.length console_log

      liftIO $ SqlWrite.store_log_info scan_resources build_step_id $
        ScanRecords.LogInfo byte_count (length lines_list) console_log

      return lines_list

  where
    aws_sess = ScanRecords.aws_sess $ ScanRecords.fetching scan_resources
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


scan_log_text ::
     [T.Text]
  -> [ScanPatterns.DbPattern]
  -> [ScanPatterns.ScanMatch]
scan_log_text lines_list patterns =
  concat $ filter (not . null) $ map apply_patterns $ zip [0..] $ map T.stripEnd lines_list
  where
    apply_patterns line_tuple = Maybe.mapMaybe (ScanUtils.apply_single_pattern line_tuple) patterns


scan_log ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> [ScanPatterns.DbPattern]
  -> IO (Either String [ScanPatterns.ScanMatch])
scan_log scan_resources build_number@(Builds.NewBuildNumber buildnum) patterns = do

  putStrLn $ "Scanning log for " ++ show (length patterns) ++ " patterns..."

  maybe_console_log <- SqlRead.read_log conn build_number
  return $ case maybe_console_log of
    Just console_log -> Right $ scan_log_text (T.lines console_log) patterns
    Nothing -> Left $ "No log found for build number " ++ show buildnum

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
