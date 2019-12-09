{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Data.Aeson                 (Value)
import           Data.Aeson.Lens            (key, _Array, _Bool, _String)
import           Data.Bifunctor             (first)
import           Data.Either                (partitionEithers)
import qualified Data.Either                as Either
import           Data.Either.Combinators    (rightToMaybe)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (intercalate)
import qualified Data.Maybe                 as Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Traversable           (for)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess
import qualified Safe

import qualified AuthStages
import qualified Builds
import qualified CircleBuild
import qualified CircleCIParse
import qualified Constants
import qualified DbHelpers
import qualified DebugUtils                 as D
import qualified FetchHelpers
import qualified MyUtils
import qualified ScanPatterns
import qualified ScanRecords
import qualified ScanUtils
import           SillyMonoids               ()
import qualified SqlRead
import qualified SqlWrite


-- | This is short so we don't
-- clog up the database with backed up requests
scanningStatementTimeoutSeconds :: Integer
scanningStatementTimeoutSeconds = 30


-- | revisit builds that may have been
-- scanned before new patterns were introduced
data RevisitationMode = NoRevisit | RevisitScanned


-- | refetch logs even if they've already been stored in the DB
data LogRefetchMode = NoRefetchLog | RefetchLog


-- | Stores scan results to database and returns them.
scanBuilds ::
     ScanRecords.ScanCatchupResources
  -> RevisitationMode
  -> LogRefetchMode
  -> Either (Set Builds.UniversalBuildId) Int
  -> IO [(DbHelpers.WithId Builds.UniversalBuild, [ScanPatterns.ScanMatch])]
scanBuilds
    scan_resources
    revisit
    refetch_logs
    whitelisted_builds_or_fetch_count = do

  rescan_matches <- case revisit of
    RevisitScanned -> do
      visited_builds_list <- case whitelisted_builds_or_fetch_count of
        Left whitelisted_build_ids -> SqlRead.getRevisitableWhitelistedBuilds
          conn
          (Set.toAscList whitelisted_build_ids)
        Right _ -> SqlRead.getRevisitableBuilds conn
      let whitelisted_visited = visited_filter visited_builds_list
      rescanVisitedBuilds
        scan_resources
        refetch_logs
        whitelisted_visited

    NoRevisit -> do
      putStrLn "NOT rescanning previously-visited builds!"
      return []

  unvisited_builds_list <- SqlRead.getUnvisitedBuildIds conn maybe_fetch_limit
  let whitelisted_unvisited = unvisited_filter unvisited_builds_list
  first_scan_matches <- processUnvisitedBuilds
    scan_resources
    whitelisted_unvisited

  return $ rescan_matches ++ first_scan_matches

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
    maybe_fetch_limit = rightToMaybe whitelisted_builds_or_fetch_count

    (visited_filter, unvisited_filter) = case whitelisted_builds_or_fetch_count of
      Right _ -> (id, id)
      Left whitelisted_builds -> (
          filter $ \(_, _, buildnum, _) -> (Builds.UniversalBuildId $ DbHelpers.db_id buildnum) `Set.member` whitelisted_builds
        , filter $ \universal_build_with_id -> (Builds.UniversalBuildId $ DbHelpers.db_id universal_build_with_id) `Set.member` whitelisted_builds)


rescanSingleBuildWrapped ::
     Builds.UniversalBuildId
  -> SqlRead.AuthDbIO (Either a T.Text)
rescanSingleBuildWrapped build_to_scan = do
  SqlRead.AuthConnection conn user <- ask
  liftIO $ rescanSingleBuild
    conn
    user
    build_to_scan
  return $ Right "Build rescan complete."


apiRescanBuilds ::
     [Builds.UniversalBuildId]
  -> SqlRead.AuthDbIO (Either T.Text Int)
apiRescanBuilds scannable_build_numbers = do
  SqlRead.AuthConnection conn user <- ask
  matches <- liftIO $ do
    scan_resources <- prepareScanResources conn $ Just user
    DbHelpers.setSessionStatementTimeout conn scanningStatementTimeoutSeconds

    scanBuilds
      scan_resources
      RevisitScanned
      NoRefetchLog
      (Left $ Set.fromList scannable_build_numbers)

  return $ Right $ length matches


-- | Does not re-download the log from AWS.
-- However, does re-fetch build info from CircleCI.
rescanSingleBuild ::
     Connection
  -> AuthStages.Username
  -> Builds.UniversalBuildId
  -> IO ()
rescanSingleBuild conn initiator build_to_scan = do

  D.debugList [
      "Rescanning build:"
    , show build_to_scan
    ]

  scan_resources <- prepareScanResources conn $ Just initiator

  parent_build <- runReaderT (SqlRead.lookupUniversalBuild build_to_scan) conn

  either_visitation_result <- getCircleCIFailedBuildInfo
    scan_resources
    (Builds.provider_buildnum $ DbHelpers.record parent_build)

  -- Note that the Left/Right convention is backwards!
  case either_visitation_result of
    Right _ -> return ()
    Left (Builds.BuildWithStepFailure build_obj _step_failure) -> do

      -- TODO It seems that this is irrelevant/redundant,
      -- since we just looked up the build from the database!
      --
      -- Perhaps instead we need to *update fields* of the stored record
      -- from information we obtained from an API fetch
      SqlWrite.storeBuildsList
        conn
        Nothing
        [DbHelpers.WithTypedId build_to_scan build_obj]

      scan_matches <- scanBuilds
        scan_resources
        RevisitScanned
        NoRefetchLog
        (Left $ Set.singleton build_to_scan)

      let total_match_count = sum $ map (length . snd) scan_matches
      D.debugList [
          "Found"
        , show total_match_count
        , "matches across"
        , show $ length scan_matches
        , "builds."
        ]


getSingleBuildUrl :: Builds.BuildNumber -> String
getSingleBuildUrl (Builds.NewBuildNumber build_number) = intercalate "/"
  [ Constants.circleci_api_base
  , show build_number
  ]


getStepFailure :: (Int, Value) -> Either Builds.BuildStepFailure ()
getStepFailure (step_index, step_val) =
  mapM_ get_failure my_array
  where
    my_array = step_val ^. key "actions" . _Array
    stepname = step_val ^. key "name" . _String

    step_fail = Left . Builds.NewBuildStepFailure stepname step_index

    get_failure x
      | x ^. key "failed" . _Bool = step_fail $
          Builds.ScannableFailure $ Builds.NewBuildFailureOutput $ x ^. key "output_url" . _String
      | x ^. key "timedout" . _Bool = step_fail Builds.BuildTimeoutFailure
      | otherwise = pure ()


prepareScanResources ::
     Connection
  -> Maybe AuthStages.Username
  -> IO ScanRecords.ScanCatchupResources
prepareScanResources conn maybe_initiator = do

  aws_sess <- Sess.newSession
  circle_sess <- Sess.newSession

  pattern_records <- SqlRead.getPatterns conn
  let patterns_by_id = DbHelpers.to_dict pattern_records

  latest_pattern_id <- SqlRead.getLatestPatternId conn
  scan_id <- SqlWrite.insertScanId conn maybe_initiator latest_pattern_id

  return $ ScanRecords.ScanCatchupResources
    scan_id
    latest_pattern_id
    patterns_by_id $ ScanRecords.FetchingResources
      conn
      aws_sess
      circle_sess


getPatternObjects ::
     ScanRecords.ScanCatchupResources
  -> [Int64]
  -> [ScanPatterns.DbPattern]
getPatternObjects scan_resources =
  Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x (ScanRecords.patterns_by_id scan_resources))


-- | This only scans patterns if they are applicable to the particular
-- failed step of this build.
-- Patterns that are not annotated with applicability will apply
-- to any step.
catchupScan ::
     ScanRecords.ScanCatchupResources
  -> LogRefetchMode
  -> Builds.BuildStepId
  -> T.Text -- ^ step name
  -> (DbHelpers.WithId Builds.UniversalBuild, Maybe Builds.BuildFailureOutput)
  -> [ScanPatterns.DbPattern]
  -> IO (Either String [ScanPatterns.ScanMatch])
catchupScan
    scan_resources
    overwrite_log
    buildstep_id
    step_name
    (universal_build_obj, maybe_console_output_url)
    scannable_patterns = do

  D.debugList [
      "\tThere are"
    , show $ length scannable_patterns
    , "scannable patterns"
    ]

  let is_pattern_applicable p = not pat_is_retired && (null appl_steps || elem step_name appl_steps)
        where
          pat_record = DbHelpers.record p
          pat_is_retired = ScanPatterns.is_retired pat_record
          appl_steps = ScanPatterns.applicable_steps pat_record
      applicable_patterns = filter is_pattern_applicable scannable_patterns

  D.debugList [
      "\t\twith"
    , show $ length applicable_patterns
    , "applicable to this step"
    ]

  -- | We only access the console log if there is at least one
  -- pattern to scan:
  case Safe.maximumMay $ map DbHelpers.db_id applicable_patterns of
    Nothing -> return $ Right []
    Just maximum_pattern_id -> runExceptT $ do

      liftIO $ D.debugStr "Starting getAndStoreLog"
      lines_list <- ExceptT $ getAndStoreLog
        scan_resources
        overwrite_log
        universal_build_obj
        buildstep_id
        maybe_console_output_url

      liftIO $ do

        D.debugStr "Finished getAndStoreLog"

        matches_and_timeouts <- scanLogText lines_list applicable_patterns

        let (timeout_list, matches) = matches_and_timeouts
        D.debugList [
            "Starting storeMatches with"
          , show $ length matches
          , "matches and"
          , show $ length timeout_list
          , "timeouts"
          ]

        D.timeThis $ SqlWrite.storeMatches scan_resources buildstep_id matches_and_timeouts

        D.debugStr "Finished storeMatches"


        D.debugStr "Starting insertLatestPatternBuildScan"

        D.timeThis $ SqlWrite.insertLatestPatternBuildScan
          scan_resources
          buildstep_id
          maximum_pattern_id

        D.debugStr "Finished insertLatestPatternBuildScan"

        return matches


rescanVisitedBuilds ::
     ScanRecords.ScanCatchupResources
  -> LogRefetchMode
  -> [(Builds.BuildStepId, T.Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
  -> IO [(DbHelpers.WithId Builds.UniversalBuild, [ScanPatterns.ScanMatch])]
rescanVisitedBuilds scan_resources should_refetch_logs visited_builds_list =

  for (zip [1::Int ..] visited_builds_list) $ \(idx, (build_step_id, step_name, universal_build_with_id, pattern_ids)) -> do
    D.debugList [
        "Visiting"
      , show idx ++ "/" ++ show visited_count
      , "previously-visited builds"
      , MyUtils.parens (show (DbHelpers.db_id universal_build_with_id)) ++ "..."
      ]

    either_matches <- catchupScan
      scan_resources
      should_refetch_logs
      build_step_id
      step_name
      (universal_build_with_id, Nothing) $
        getPatternObjects scan_resources pattern_ids

    return (universal_build_with_id, Either.fromRight [] either_matches)

  where
    visited_count = length visited_builds_list


-- | This function stores a record to the database
-- immediately upon build visitation. We do this instead of waiting
-- until the end so that we can resume progress if the process is
-- interrupted.
processUnvisitedBuilds ::
     ScanRecords.ScanCatchupResources
  -> [DbHelpers.WithId Builds.UniversalBuild]
  -> IO [(DbHelpers.WithId Builds.UniversalBuild, [ScanPatterns.ScanMatch])]
processUnvisitedBuilds scan_resources unvisited_builds_list =

  for (zip [1::Int ..] unvisited_builds_list) $ \(idx, universal_build_obj) -> do
    D.debugList [
        "Visiting"
      , show idx ++ "/" ++ show unvisited_count
      , "unvisited builds..."
      ]

    visitation_result <- getCircleCIFailedBuildInfo
      scan_resources
      (Builds.provider_buildnum $ DbHelpers.record universal_build_obj)

    let pair = (universal_build_obj, visitation_result)
    build_step_id <- SqlWrite.insertBuildVisitation scan_resources pair

    either_matches <- case visitation_result of
      Right _ -> return $ Right []
      Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure step_name _step_index mode)) -> case mode of
        Builds.BuildTimeoutFailure             -> return $ Right []
        Builds.ScannableFailure failure_output -> catchupScan
          scan_resources
          RefetchLog -- Re-download parameter is irrelevant, since this is the first time visiting the build
          build_step_id
          step_name
          (universal_build_obj, Just failure_output) $
            ScanRecords.getPatternsWithId scan_resources

    return (universal_build_obj, Either.fromRight [] either_matches)

  where
    unvisited_count = length unvisited_builds_list


-- | Determines which step of the build failed and stores
-- the console log to disk, if there is one.
--
-- Note that this function is a bit backwards in its use of Either;
-- here, the *expected* outcome is a Left, whereas a Right is the "bad" condition.
-- Rationale: we're searching a known-failed build for failures, so not finding a failure is unexpected.
-- We make use of Either's short-circuting to find the *first* failure.
getCircleCIFailedBuildInfo ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildNumber
  -> IO (Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
getCircleCIFailedBuildInfo scan_resources build_number = do

  D.debugList ["Fetching from:", fetch_url]

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
      first (Builds.BuildWithStepFailure $ CircleBuild.toBuild build_number r) $ mapM_ getStepFailure $ zip [0..] steps_list
      return ScanRecords.NoFailedSteps

    Left err_message -> do
      let fail_string = "PROBLEM: Failed in getCircleCIFailedBuildInfo with message: " ++ err_message
      return $ ScanRecords.NetworkProblem fail_string

  where
    fetch_url = getSingleBuildUrl build_number
    opts = defaults & header "Accept" .~ [Constants.jsonMimeType]
    sess = ScanRecords.circle_sess $ ScanRecords.fetching scan_resources


-- | This function strips all ANSI escape codes from the console log before storage.
getAndStoreLog ::
     ScanRecords.ScanCatchupResources
  -> LogRefetchMode
  -> DbHelpers.WithId Builds.UniversalBuild
  -> Builds.BuildStepId
  -> Maybe Builds.BuildFailureOutput
  -> IO (Either String [LT.Text])
getAndStoreLog
    scan_resources
    overwrite
    universal_build
    build_step_id
    maybe_failed_build_output = do

  maybe_console_log <- case overwrite of
    RefetchLog   -> return Nothing
    NoRefetchLog -> runReaderT (SqlRead.readLog build_step_id) conn

  case maybe_console_log of
    Just console_log -> return $ Right $ LT.lines console_log  -- Log was already fetched
    Nothing -> runExceptT $ do
      download_url <- ExceptT $ case maybe_failed_build_output of
        Just failed_build_output -> return $ Right $ Builds.log_url failed_build_output
        Nothing -> do
          visitation_result <- getCircleCIFailedBuildInfo
            scan_resources
            (Builds.provider_buildnum $ DbHelpers.record universal_build)

          case visitation_result of
            Right _ -> return $ Left "This build didn't have a console log!"
            Left (Builds.BuildWithStepFailure build_obj (Builds.NewBuildStepFailure _step_name _step_index mode)) -> do

              -- Store the build metadata again, because the first time may have been
              -- obtained through the GitHub notification, which lacks build duration
              -- and branch name.
              SqlWrite.storeBuildsList
                conn
                Nothing
                [DbHelpers.WithTypedId universal_build_id build_obj]

              return $ case mode of
                Builds.BuildTimeoutFailure             -> Left "This build didn't have a console log because it was a timeout!"
                Builds.ScannableFailure failure_output -> Right $ Builds.log_url failure_output

      liftIO $ D.debugList [
          "Downloading log from:"
        , T.unpack download_url
        ]

      log_download_result <- ExceptT $ FetchHelpers.safeGetUrl $ Sess.get aws_sess $ T.unpack download_url


      parsed <- CircleCIParse.doParse scan_resources build_step_id log_download_result

      liftIO $ D.debugStr "Finished getAndStoreLog."

      return parsed

  where
    aws_sess = ScanRecords.aws_sess $ ScanRecords.fetching scan_resources
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources

    universal_build_id = Builds.UniversalBuildId $ DbHelpers.db_id universal_build


-- | Even though scanning for patterns in text is ostensibly a "pure" operation,
-- we introduce IO so we can see incremental progress, and perhaps
-- interrupt a long-running computation.
--
-- NOTE: Skips lines that are over 5000 characters
-- to avoid bad regex behavior.
scanLogText ::
     [LT.Text]
  -> [ScanPatterns.DbPattern]
  -> IO ([ScanUtils.PatternScanTimeout], [ScanPatterns.ScanMatch])
scanLogText lines_list patterns = do
  result_tuples <- for input_pairs $ \num_line_pair -> do
    {-
    D.debugList [
        "Scanning Line number"
      , show num
      , "/"
      , show $ length lines_list
      , "which has length"
      , show $ LT.length line
      ]

    D.debugList [
        "\tExcerpt:"
      , take 500 $ LT.unpack line
      ]
    -}

    apply_patterns num_line_pair


  let final_matches = concat $ filter (not . null) $ map snd result_tuples

  return (concatMap fst result_tuples, final_matches)
  where

    input_pairs = zip [0 ..] $ map LT.stripEnd lines_list
--    apply_patterns line_tuple = Maybe.mapMaybe (ScanUtils.applySinglePattern line_tuple) patterns
    apply_patterns line_tuple = do
      pattern_result_eithers <- for patterns $ \pat -> do
        ans <- ScanUtils.applySinglePatternIO line_tuple pat
        case ans of
          Right blah -> do
--            D.debugStr "completed on time."
            return $ Right blah
          Left foo -> do
--            D.debugStr "NOT completed on time."
            return $ Left foo

      let (timedout, match_maybes) = partitionEithers pattern_result_eithers
          flattened_matches = Maybe.mapMaybe ScanUtils.convertMatchAnswerToMaybe match_maybes

      return (timedout, flattened_matches)
