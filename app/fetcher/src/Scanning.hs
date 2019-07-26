{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens            (key, _Array, _Bool, _String)
import           Data.Bifunctor             (first)
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
import           Text.Regex                 (mkRegex, subRegex)
import           Text.Regex.Posix.Wrap      (Regex)

import qualified AuthStages
import qualified Builds
import qualified CircleBuild
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
scanBuilds ::
     ScanRecords.ScanCatchupResources
  -> Bool -- ^ revisit
  -> Either (Set Builds.BuildNumber) Int
  -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
scanBuilds scan_resources revisit whitelisted_builds_or_fetch_count = do

  rescan_matches <- if revisit
    then do
      visited_builds_list <- SqlRead.getRevisitableBuilds conn
      let whitelisted_visited = visited_filter visited_builds_list
      rescanVisitedBuilds scan_resources whitelisted_visited
    else do
      putStrLn "NOT rescanning previously-visited builds!"
      return []

  unvisited_builds_list <- SqlRead.getUnvisitedBuildIds conn maybe_fetch_limit
  let whitelisted_unvisited = unvisited_filter unvisited_builds_list
  first_scan_matches <- processUnvisitedBuilds scan_resources whitelisted_unvisited

  return $ rescan_matches ++ first_scan_matches

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
    maybe_fetch_limit = rightToMaybe whitelisted_builds_or_fetch_count

    (visited_filter, unvisited_filter) = case whitelisted_builds_or_fetch_count of
      Right _ -> (id, id)
      Left whitelisted_builds -> (
          filter $ \(_, _, buildnum, _) -> buildnum `Set.member` whitelisted_builds
        , filter (`Set.member` whitelisted_builds)
        )


-- TODO RESTORE THIS
{-

-- | Note that the Left/Right convention is backwards!
rescanSingleBuild ::
     DbHelpers.DbConnectionData
  -> AuthStages.Username
  -> Builds.BuildNumber
  -> IO ()
rescanSingleBuild db_connection_data initiator build_to_scan = do
  putStrLn $ "Rescanning build: " ++ show build_to_scan
  conn <- DbHelpers.get_connection db_connection_data
  scan_resources <- prepareScanResources conn $ Just initiator

  either_visitation_result <- getFailedBuildInfo scan_resources build_to_scan
  case either_visitation_result of
    Right _ -> return ()
    Left (Builds.BuildWithStepFailure build_obj _step_failure) -> do

      SqlWrite.storeBuildsList conn [build_obj]

      scan_matches <- scanBuilds scan_resources True $ Left $ Set.singleton build_to_scan

      let total_match_count = sum $ map (length . snd) scan_matches
      putStrLn $ unwords [
          "Found"
        , show total_match_count
        , "matches across"
        , show $ length scan_matches
        , "builds."
        ]
      return ()
-}

getSingleBuildUrl :: Builds.BuildNumber -> String
getSingleBuildUrl (Builds.NewBuildNumber build_number) = intercalate "/"
  [ Constants.circleci_api_base
  , show build_number
  ]


getStepFailure :: Value -> Either Builds.BuildStepFailure ()
getStepFailure step_val =
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


prepareScanResources :: Connection -> Maybe AuthStages.Username -> IO ScanRecords.ScanCatchupResources
prepareScanResources conn maybe_initiator = do

  aws_sess <- Sess.newSession
  circle_sess <- Sess.newSession

  pattern_records <- SqlRead.getPatterns conn
  let patterns_by_id = DbHelpers.to_dict pattern_records

  latest_pattern_id <- SqlRead.getLatestPatternId conn
  scan_id <- SqlWrite.insert_scan_id conn maybe_initiator latest_pattern_id

  return $ ScanRecords.ScanCatchupResources
    scan_id
    latest_pattern_id
    patterns_by_id $ ScanRecords.FetchingResources
      conn
      aws_sess
      circle_sess


getPatternObjects :: ScanRecords.ScanCatchupResources -> [Int64] -> [ScanPatterns.DbPattern]
getPatternObjects scan_resources =
  Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x (ScanRecords.patterns_by_id scan_resources))


-- | This only scans patterns if they are applicable to the particular
-- failed step of this build.
-- Patterns that are not annotated with applicability will apply
-- to any step.
catchupScan ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> T.Text -- ^ step name
  -> (Builds.BuildNumber, Maybe Builds.BuildFailureOutput)
  -> [ScanPatterns.DbPattern]
  -> IO (Either String [ScanPatterns.ScanMatch])
catchupScan
    scan_resources
    buildstep_id
    step_name
    (buildnum, maybe_console_output_url)
    scannable_patterns = do

  putStrLn $ "\tThere are " ++ show (length scannable_patterns) ++ " scannable patterns"

  let is_pattern_applicable p = not pat_is_retired && (null appl_steps || elem step_name appl_steps)
        where
          pat_record = DbHelpers.record p
          pat_is_retired = ScanPatterns.is_retired pat_record
          appl_steps = ScanPatterns.applicable_steps pat_record
      applicable_patterns = filter is_pattern_applicable scannable_patterns

  putStrLn $ "\t\twith " ++ show (length applicable_patterns) ++ " applicable to this step"

  -- | We only access the console log if there is at least one
  -- pattern to scan:
  case Safe.maximumMay $ map DbHelpers.db_id applicable_patterns of
    Nothing -> return $ Right []
    Just maximum_pattern_id -> runExceptT $ do

      lines_list <- ExceptT $ getAndStoreLog
        scan_resources
        False
        buildnum
        buildstep_id
        maybe_console_output_url

      let matches = scanLogText lines_list applicable_patterns

      liftIO $ do
        SqlWrite.storeMatches scan_resources buildstep_id buildnum matches
        SqlWrite.insert_latest_pattern_build_scan scan_resources buildnum maximum_pattern_id

      return matches


rescanVisitedBuilds ::
     ScanRecords.ScanCatchupResources
  -> [(Builds.BuildStepId, T.Text, Builds.BuildNumber, [Int64])]
  -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
rescanVisitedBuilds scan_resources visited_builds_list =

  for (zip [1::Int ..] visited_builds_list) $ \(idx, (build_step_id, step_name, build_num, pattern_ids)) -> do
    putStrLn $ "Visiting " ++ show idx ++ "/" ++ show visited_count ++ " previously-visited builds (" ++ show build_num ++ ")..."

    either_matches <- catchupScan scan_resources build_step_id step_name (build_num, Nothing) $
      getPatternObjects scan_resources pattern_ids

    return (build_num, Either.fromRight [] either_matches)

  where
    visited_count = length visited_builds_list


-- | This function stores a record to the database
-- immediately upon build visitation. We do this instead of waiting
-- until the end so that we can resume progress if the process is
-- interrupted.
processUnvisitedBuilds :: ScanRecords.ScanCatchupResources -> [Builds.BuildNumber] -> IO [(Builds.BuildNumber, [ScanPatterns.ScanMatch])]
processUnvisitedBuilds scan_resources unvisited_builds_list =

  for (zip [1::Int ..] unvisited_builds_list) $ \(idx, build_num) -> do
    putStrLn $ unwords [
        "Visiting"
      , show idx ++ "/" ++ show unvisited_count
      , "unvisited builds..."
      ]
    visitation_result <- getFailedBuildInfo scan_resources build_num

    let pair = (build_num, visitation_result)
    build_step_id <- SqlWrite.insert_build_visitation scan_resources pair

    either_matches <- case visitation_result of
      Right _ -> return $ Right []
      Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure step_name mode)) -> case mode of
        Builds.BuildTimeoutFailure             -> return $ Right []
        Builds.ScannableFailure failure_output ->
          catchupScan scan_resources build_step_id step_name (build_num, Just failure_output) $
            ScanRecords.get_patterns_with_id scan_resources

    return (build_num, Either.fromRight [] either_matches)

  where
    unvisited_count = length unvisited_builds_list


-- | Determines which step of the build failed and stores
-- the console log to disk, if there is one.
--
-- Note that this function is a bit backwards in its use of Either;
-- here, the *expected* outcome is a Left, whereas a Right is the "bad" condition.
-- Rationale: we're searching a known-failed build for failures, so not finding a failure is unexpected.
-- We make use of Either's short-circuting to find the *first* failure.
getFailedBuildInfo ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> IO (Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
getFailedBuildInfo scan_resources build_number = do

  putStrLn $ "Fetching from: " ++ fetch_url

  either_r <- runExceptT $ do
    r <- ExceptT $ FetchHelpers.safeGetUrl $ Sess.getWith opts sess fetch_url
    jsonified_r <- NW.asJSON r
    return $ jsonified_r ^. NW.responseBody

  return $ case either_r of
    Right r -> do

      let steps_list = CircleBuild.steps r

      -- We expect to short circuit here and return a build step failure,
      -- but if we don't, we proceed
      -- to the NoFailedSteps return value.
      first (Builds.BuildWithStepFailure $ CircleBuild.toBuild build_number r) $ mapM_ getStepFailure steps_list
      return ScanRecords.NoFailedSteps

    Left err_message -> do
      let fail_string = "PROBLEM: Failed in getFailedBuildInfo with message: " ++ err_message
      return $ ScanRecords.NetworkProblem fail_string

  where
    fetch_url = getSingleBuildUrl build_number
    opts = defaults & header "Accept" .~ [Constants.json_mime_type]
    sess = ScanRecords.circle_sess $ ScanRecords.fetching scan_resources


-- | This function strips all ANSI escape codes from the console log before storage.
getAndStoreLog ::
     ScanRecords.ScanCatchupResources
  -> Bool -- ^ overwrite existing log
  -> Builds.BuildNumber
  -> Builds.BuildStepId
  -> Maybe Builds.BuildFailureOutput
  -> IO (Either String [T.Text])
getAndStoreLog
    scan_resources
    overwrite
    build_number
    build_step_id
    maybe_failed_build_output = do

  maybe_console_log <- if overwrite
    then return Nothing
    else SqlRead.read_log conn build_number

  case maybe_console_log of
    Just console_log -> return $ Right $ T.lines console_log  -- Log was already fetched
    Nothing -> runExceptT $ do
      download_url <- ExceptT $ case maybe_failed_build_output of
        Just failed_build_output -> return $ Right $ Builds.log_url failed_build_output
        Nothing -> do
          visitation_result <- getFailedBuildInfo scan_resources build_number

          return $ case visitation_result of
            Right _ -> Left "This build didn't have a console log!"
            Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure _step_name mode)) -> case mode of
              Builds.BuildTimeoutFailure             -> Left "This build didn't have a console log because it was a timeout!"
              Builds.ScannableFailure failure_output -> Right $ Builds.log_url failure_output


      liftIO $ putStrLn $ "Downloading log from: " ++ T.unpack download_url

      log_download_result <- ExceptT $ FetchHelpers.safeGetUrl $ Sess.get aws_sess $ T.unpack download_url

      liftIO $ putStrLn "Log downloaded."

      let parent_elements = log_download_result ^. NW.responseBody . _Array
          -- for some reason the log is sometimes split into sections, so we concatenate all of the "out" elements
          pred x = x ^. key "type" . _String == "out"
          output_elements = filter pred $ V.toList parent_elements

          raw_console_log = mconcat $ map (\x -> x ^. key "message" . _String) output_elements

          ansi_stripped_log = T.pack $ filterAnsi $ T.unpack raw_console_log

          lines_list = T.lines ansi_stripped_log
          byte_count = T.length ansi_stripped_log

      liftIO $ do
        putStrLn "Storing log to database..."

        SqlWrite.store_log_info scan_resources build_step_id $
          ScanRecords.LogInfo byte_count (length lines_list) ansi_stripped_log

        putStrLn "Log stored."

      return lines_list

  where
    aws_sess = ScanRecords.aws_sess $ ScanRecords.fetching scan_resources
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


-- | Strips bold markup and coloring
ansiRegex :: Text.Regex.Posix.Wrap.Regex
ansiRegex = mkRegex "\x1b\\[([0-9;]*m|K)"


-- | TODO: Consider using attoparsec instead of regex:
-- https://pl-rants.net/posts/regexes-and-combinators/
filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex line ""


scanLogText ::
     [T.Text]
  -> [ScanPatterns.DbPattern]
  -> [ScanPatterns.ScanMatch]
scanLogText lines_list patterns =
  concat $ filter (not . null) $ zipWith (curry apply_patterns) [0 ..] $ map T.stripEnd lines_list
  where
    apply_patterns line_tuple = Maybe.mapMaybe (ScanUtils.applySinglePattern line_tuple) patterns


scanLog ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> [ScanPatterns.DbPattern]
  -> IO (Either String [ScanPatterns.ScanMatch])
scanLog scan_resources build_number@(Builds.NewBuildNumber buildnum) patterns = do

  putStrLn $ unwords [
      "Scanning log for"
    , show $ length patterns
    , "patterns..."
    ]

  maybe_console_log <- SqlRead.read_log conn build_number
  return $ case maybe_console_log of
    Just console_log -> Right $ scanLogText (T.lines console_log) patterns
    Nothing -> Left $ unwords [
        "No log found for build number"
      , show buildnum
      ]

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
