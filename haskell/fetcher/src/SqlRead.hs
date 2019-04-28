{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad              (forM)
import           Data.Aeson
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List.Split            (splitOn)
import qualified Data.Maybe                 as Maybe
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time                  (UTCTime)
import           Data.Time.Calendar         (Day)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.Int                    (Int64)
import           System.Directory           (doesFileExist)
import qualified System.DiskSpace           as DiskSpace
import           System.Process             (readProcess)

import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified ScanPatterns


data ScanScope = NewScanScope {
    build_number       :: Builds.BuildNumber -- ^ used to retrieve the console log file from disk
  , build_step_id      :: Builds.BuildStepId
  , unscanned_patterns :: [ScanPatterns.DbPattern]
  }


-- | Some scan patterns only apply to certain build steps, so we
-- filter those in this function.
get_unscanned_build_patterns :: Connection -> HashMap Int64 ScanPatterns.Pattern -> IO [SqlRead.ScanScope]
get_unscanned_build_patterns conn patterns_by_id = do

  unscanned_patterns_list <- query_ conn sql

  buildnum_patt_id_tuples <- forM unscanned_patterns_list $ \(build_num, step_id, step_name, comma_sep_pattern_ids) -> let
        pattern_ids = Set.fromList $ map read $ splitOn "," $ comma_sep_pattern_ids
        patterns = Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x patterns_by_id) $ Set.toList pattern_ids
        applicability_predicate p = null pattern_steps || step_name `elem` pattern_steps
          where
            pattern_steps = ScanPatterns.applicable_steps $ DbHelpers.record p
        filtered_patterns = filter applicability_predicate patterns
    in return $ NewScanScope (Builds.NewBuildNumber build_num) (Builds.NewBuildStepId step_id) filtered_patterns

  return buildnum_patt_id_tuples
  where
    sql = "SELECT build_num, build_steps.id, build_steps.name, unscanned_patts FROM unscanned_patterns JOIN build_steps ON unscanned_patterns.build_num = build_steps.build ORDER BY patt_count"


get_patterns :: Connection -> IO [ScanPatterns.DbPattern]
get_patterns conn = do

  patterns_rows <- query_ conn patterns_sql

  forM patterns_rows $ \(pattern_id, is_regex, pattern_text, description) -> do

    tags_rows <- query conn tags_sql (Only pattern_id)
    tags_list <- forM tags_rows $ \(Only tag_text) -> return tag_text

    steps_rows <- query conn applicable_steps_sql (Only pattern_id)
    steps_list <- forM steps_rows $ \(Only step_text) -> return step_text

    let expression_obj = if is_regex
          then ScanPatterns.RegularExpression $ encodeUtf8 pattern_text
          else ScanPatterns.LiteralExpression pattern_text
        inner_pattern = ScanPatterns.NewPattern expression_obj description tags_list steps_list
        outer_pattern = DbHelpers.WithId pattern_id inner_pattern

    return outer_pattern

  where
    patterns_sql = "SELECT id, regex, expression, description FROM patterns ORDER BY description;"

    tags_sql = "SELECT tag FROM pattern_tags WHERE pattern = ?;"
    applicable_steps_sql = "SELECT step_name FROM pattern_step_applicability WHERE pattern = ?;"


get_unvisited_build_ids :: Connection -> Int -> IO [Builds.BuildNumber]
get_unvisited_build_ids conn limit = do
  rows <- query conn sql (Only limit)
  forM rows $ \(Only num) -> return $ Builds.NewBuildNumber num
  where
    sql = "SELECT build_num FROM unvisited_builds ORDER BY build_NUM DESC LIMIT ?;"


data PatternId = NewPatternId Int64


get_latest_pattern_id :: Connection -> IO PatternId
get_latest_pattern_id conn = do
  [Only pattern_id] <- query_ conn sql
  return $ NewPatternId pattern_id
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1"


query_builds :: IO [Builds.Build]
query_builds = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT build_num, vcs_revision, queued_at, job_name FROM builds"
  forM xs $ \(buildnum, vcs_rev, queuedat, jobname) ->
    return $ Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queuedat jobname


data ApiResponse a = ApiResponse {
    rows :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (ApiResponse a)


dropUnderscore = defaultOptions {fieldLabelModifier = drop 1}


data JobApiRecord = JobApiRecord {
    _name :: Text
  , _data :: [Int]
  } deriving Generic

instance ToJSON JobApiRecord where
  toJSON = genericToJSON dropUnderscore


api_jobs :: IO (ApiResponse JobApiRecord)
api_jobs = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT job_name, freq FROM job_failure_frequencies"
  inners <- forM xs $ \(jobname, freq) ->
    return $ JobApiRecord jobname [freq]

  return $ ApiResponse inners


data PieSliceApiRecord = PieSliceApiRecord {
    _name :: Text
  , _y    :: Integer
  } deriving Generic

instance ToJSON PieSliceApiRecord where
  toJSON = genericToJSON dropUnderscore


api_step :: IO (ApiResponse PieSliceApiRecord)
api_step = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT name, COUNT(*) AS freq FROM build_steps WHERE name IS NOT NULL GROUP BY name ORDER BY freq DESC"
  inners <- forM xs $ \(stepname, freq) ->
    return $ PieSliceApiRecord stepname freq

  return $ ApiResponse inners


-- | Note that Highcharts expects the dates to be in ascending order
api_failed_commits_by_day :: IO (ApiResponse (Day, Int))
api_failed_commits_by_day = do
  conn <- DbHelpers.get_connection

  inners <- query_ conn "SELECT queued_at::date AS date, COUNT(*) FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM builds GROUP BY vcs_revision) foo GROUP BY date ORDER BY date ASC"

  return $ ApiResponse inners


data SummaryStats = SummaryStats {
    _failed_builds      :: Int
  , _visited_builds     :: Int
  , _explained_failures :: Int
  , _timed_out_steps    :: Int
  , _steps_with_a_match :: Int
  } deriving Generic

instance ToJSON SummaryStats where
  toJSON = genericToJSON dropUnderscore


api_summary_stats = do
  conn <- DbHelpers.get_connection

  [Only build_count] <- query_ conn "SELECT COUNT(*) FROM builds"
  [Only visited_count] <- query_ conn "SELECT COUNT(*) FROM build_steps"
  [Only explained_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE name IS NOT NULL"
  [Only timeout_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE is_timeout"
  [Only matched_steps_count] <- query_ conn "SELECT COUNT(*) FROM (SELECT build_step FROM public.matches GROUP BY build_step) x"

  return $ SummaryStats build_count visited_count explained_count timeout_count matched_steps_count


api_disk_space = do

  dir_exists <- doesFileExist Constants.url_cache_basedir

  cache_bytes <- if dir_exists
    then do
      output <- readProcess "/usr/bin/du" ["--bytes", Constants.url_cache_basedir] ""
      return $ read $ takeWhile (\x -> x /= '\t') output
    else return 0

  let avail_space_reference_dir = if dir_exists
        then Constants.url_cache_basedir
        else "/"

  avail_bytes <- DiskSpace.getAvailSpace avail_space_reference_dir

  return $ [
      PieSliceApiRecord "Available" avail_bytes
    , PieSliceApiRecord "Consumed" cache_bytes
    ]


data PatternRecord = PatternRecord {
    _id          :: Int64
  , _is_regex    :: Bool
  , _pattern     :: Text
  , _description :: Text
  , _frequency   :: Int
  , _last        :: Maybe UTCTime
  , _earliest    :: Maybe UTCTime
  , _tags        :: Text
  } deriving Generic

instance ToJSON PatternRecord where
  toJSON = genericToJSON dropUnderscore

api_single_pattern :: Int ->  IO [PatternRecord]
api_single_pattern pattern_id = do
  conn <- DbHelpers.get_connection

  xs <- query conn "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags FROM pattern_frequency_summary WHERE id = ?" (Only pattern_id)
  inners <- forM xs $ \(a, b, c, d, e, f, g, h) ->
    return $ PatternRecord a b c d e f g h

  return inners


api_patterns :: IO [PatternRecord]
api_patterns = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags FROM pattern_frequency_summary"
  inners <- forM xs $ \(a, b, c, d, e, f, g, h) ->
    return $ PatternRecord a b c d e f g h

  return inners


data PatternOccurrences = PatternOccurrences {
    _build_number :: Int64
  , _build_step   :: Text
  , _line_number  :: Int
  , _line_text    :: Text
  , _span_start   :: Int
  , _span_end     :: Int
  } deriving Generic

instance ToJSON PatternOccurrences where
  toJSON = genericToJSON dropUnderscore


get_pattern_matches :: Int -> IO [PatternOccurrences]
get_pattern_matches pattern_id = do
  rows <- get_pattern_occurrence_rows pattern_id
  return $ map txform rows

  where
    txform (Builds.NewBuildNumber buildnum, stepname, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = PatternOccurrences buildnum stepname line_number line_text start end


get_pattern_occurrence_rows :: Int -> IO [(Builds.BuildNumber, Text, ScanPatterns.MatchDetails)]
get_pattern_occurrence_rows pattern_id = do

  conn <- DbHelpers.get_connection

  xs <- query conn "SELECT build_steps.build AS build_num, build_steps.name, line_number, line_text, span_start, span_end FROM (SELECT * FROM matches WHERE pattern = ?) foo JOIN build_steps ON build_steps.id = foo.build_step ORDER BY build_num DESC" (Only pattern_id)

  inners <- forM xs $ \(buildnum, stepname, line_number, line_text, span_start, span_end) ->
    return $ (Builds.NewBuildNumber buildnum, stepname, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

  return inners

