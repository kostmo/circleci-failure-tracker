{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import           Control.Lens               hiding ((<.>))
import           Control.Monad              (unless)
import           Data.Aeson                 (Value, decode, encode)
import           Data.Aeson.Lens            (key, _Array, _Bool, _Integral,
                                             _String)
import           Data.Foldable              (for_)
import           Data.List                  (intercalate)
import           Data.Maybe                 (Maybe)
import qualified Data.Maybe                 as Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Internal.Search  as Search
import qualified Data.Text.IO               as TIO
import           Data.Traversable           (for)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq
import qualified Network.Wreq.Session       as Sess
import qualified Safe
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.FilePath
import           Text.Regex.Base
import           Text.Regex.PCRE            ((=~~))

import           Builds
import qualified Constants
import qualified DbHelpers
import qualified Helpers
import qualified ScanPatterns
import           SillyMonoids               ()
import qualified SqlRead
import qualified SqlWrite


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


get_step_failure :: Value -> Either BuildStepFailure ()
get_step_failure step_val =
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


get_console_url :: (BuildNumber, Maybe BuildStepFailure) -> Maybe (BuildNumber, BuildFailureOutput)
get_console_url (build_number, maybe_thing) = case maybe_thing of
  Nothing -> Nothing
  Just (NewBuildStepFailure step_name mode) -> case mode of
      BuildTimeoutFailure             -> Nothing
      ScannableFailure failure_output -> Just (build_number, failure_output)


-- | TODO We may want to interleave the console log downloads from AWS
-- with the build details visitations from CircleCI.
store_build_failure_metadata :: Connection -> [BuildNumber] -> IO Int64
store_build_failure_metadata conn unvisited_builds_list = do

  sess <- Sess.newSession
  visitations <- for unvisited_builds_list $ \build_num -> do
    result <- Scanning.get_failed_build_info sess build_num
    let foo = case result of
          Right _ -> Nothing
          Left x  -> Just x
    return (build_num, foo)

  store_all_logs $ Maybe.mapMaybe get_console_url visitations

  SqlWrite.insert_build_visitations conn visitations


-- | Determines which step of the build failed and stores
-- the console log to disk, if there is one.
get_failed_build_info :: Sess.Session -> BuildNumber -> IO (Either BuildStepFailure ())
get_failed_build_info sess build_number = do

  putStrLn $ "Fetching from: " ++ fetch_url

  r <- Sess.getWith opts sess fetch_url
  let steps_list = r ^. responseBody . key "steps" . _Array

  return $ mapM_ get_step_failure steps_list

  where
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

  -- TODO shouldn't even need to perform this check, because upstream we've already
  -- filtered out pre-cached build logs via the SQL query
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


scan_all_logs :: Connection -> [SqlRead.ScanScope] -> IO Int
scan_all_logs conn scannable = do

  scan_id <- SqlWrite.insert_scan_id conn
  matches <- mapM scan_log scannable

  mapM_ (SqlWrite.store_matches conn) matches

  return $ length matches


scan_log :: SqlRead.ScanScope -> IO (SqlRead.ScanScope, [ScanPatterns.ScanMatch])
scan_log scan_scope = do

  putStrLn $ "Scanning log: " ++ full_filepath

  console_log <- TIO.readFile full_filepath
  let result = filter (not . null) $ map apply_patterns $ zip [0..] $ map T.stripEnd $ T.lines console_log
  return (scan_scope, concat result)

  where
    apply_patterns line_tuple = Maybe.mapMaybe (apply_single_pattern line_tuple) $ SqlRead.unscanned_patterns scan_scope

    apply_single_pattern (line_number, line) db_pattern = match_partial <$> match_span
      where

        match_span = case ScanPatterns.expression pattern_obj of
          ScanPatterns.RegularExpression regex_text -> case ((T.unpack line) =~~ regex_text :: Maybe (MatchOffset, MatchLength)) of
            Just (match_offset, match_length) -> Just $ ScanPatterns.NewMatchSpan match_offset (match_offset + match_length)
            Nothing -> Nothing
          ScanPatterns.LiteralExpression literal_text -> case Safe.headMay (Search.indices literal_text line) of
            Just first_index -> Just $ ScanPatterns.NewMatchSpan first_index (first_index + T.length literal_text)
            Nothing -> Nothing


        match_partial = ScanPatterns.NewScanMatch db_pattern line line_number
        pattern_obj = DbHelpers.record db_pattern

    full_filepath = gen_log_path $ SqlRead.build_number scan_scope
