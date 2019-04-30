{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad                        (forM)
import           Data.Aeson
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.Maybe                           as Maybe
import           Data.Text                            (Text)
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Time                            (UTCTime)
import           Data.Time.Calendar                   (Day)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           GHC.Generics
import           GHC.Int                              (Int64)

import qualified Builds
import qualified DbHelpers
import qualified ScanPatterns
import qualified ScanRecords
import qualified WebApi



data ScanScope = NewScanScope {
    build_number       :: Builds.BuildNumber -- ^ used to retrieve the console log file from disk
  , build_step_id      :: Builds.BuildStepId
  , unscanned_patterns :: [ScanPatterns.DbPattern]
  }


-- | Some scan patterns only apply to certain build steps, so we
-- filter those in this function.
get_unscanned_patterns_for_build :: ScanRecords.ScanCatchupResources -> Builds.BuildNumber -> IO [ScanPatterns.DbPattern]
get_unscanned_patterns_for_build scan_resources (Builds.NewBuildNumber _build_number) = do

  unscanned_patterns_list <- query_ (ScanRecords.db_conn scan_resources) sql

  let patt_ids = map (\(Only pattern_id) -> pattern_id) unscanned_patterns_list
      patterns = Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x (ScanRecords.patterns_by_id scan_resources)) patt_ids

  return patterns
  where
    sql = "SELECT id FROM patterns"


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


get_latest_pattern_id :: Connection -> IO ScanRecords.PatternId
get_latest_pattern_id conn = do
  [Only pattern_id] <- query_ conn sql
  return $ ScanRecords.NewPatternId pattern_id
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1"


query_builds :: IO [Builds.Build]
query_builds = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT build_num, vcs_revision, queued_at, job_name, branch FROM builds"
  forM xs $ \(buildnum, vcs_rev, queuedat, jobname, branch) ->
    return $ Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queuedat jobname branch




api_jobs :: IO (WebApi.ApiResponse WebApi.JobApiRecord)
api_jobs = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT job_name, freq FROM job_failure_frequencies"
  inners <- forM xs $ \(jobname, freq) ->
    return $ WebApi.JobApiRecord jobname [freq]

  return $ WebApi.ApiResponse inners



api_step :: IO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
api_step = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT name, COUNT(*) AS freq FROM build_steps WHERE name IS NOT NULL GROUP BY name ORDER BY freq DESC"
  inners <- forM xs $ \(stepname, freq) ->
    return $ WebApi.PieSliceApiRecord stepname freq

  return $ WebApi.ApiResponse inners


-- | Note that Highcharts expects the dates to be in ascending order
api_failed_commits_by_day :: IO (WebApi.ApiResponse (Day, Int))
api_failed_commits_by_day = do
  conn <- DbHelpers.get_connection

  inners <- query_ conn "SELECT queued_at::date AS date, COUNT(*) FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM builds GROUP BY vcs_revision) foo GROUP BY date ORDER BY date ASC"

  return $ WebApi.ApiResponse inners



list_builds :: Query -> IO [WebApi.BuildNumberRecord]
list_builds q = do
  conn <- DbHelpers.get_connection

  inners <- query_ conn sql

  return $ map (\(Only x) -> WebApi.BuildNumberRecord $ Builds.NewBuildNumber x) inners
  where
    sql = q


api_unmatched_builds :: IO [WebApi.BuildNumberRecord]
api_unmatched_builds = list_builds sql
  where
    sql = "SELECT * FROM unattributed_failed_builds"


api_idiopathic_builds :: IO [WebApi.BuildNumberRecord]
api_idiopathic_builds = list_builds sql
  where
    sql = "SELECT * FROM idiopathic_build_failures"


api_random_scannable_build :: IO WebApi.BuildNumberRecord
api_random_scannable_build = do
  conn <- DbHelpers.get_connection
  [Only buildnum] <- query_ conn sql
  return $ WebApi.BuildNumberRecord $ Builds.NewBuildNumber buildnum
  where
    sql = "SELECT build_num FROM scannable_build_steps OFFSET floor(random()*(SELECT COUNT(*) FROM scannable_build_steps)) LIMIT 1"


list_flat :: FromField a => Query -> Text -> IO [a]
list_flat q t = do
  conn <- DbHelpers.get_connection
  inners <- query conn sql (Only t)
  return $ map (\(Only x) -> x) inners
  where
    sql = q


api_list_tags :: Text -> IO [Text]
api_list_tags = list_flat sql
  where
    sql = "SELECT tag FROM (SELECT tag, COUNT(*) AS freq FROM pattern_tags GROUP BY tag ORDER BY freq DESC, tag ASC) foo WHERE tag ILIKE CONCAT(?,'%')"


api_list_steps :: Text -> IO [Text]
api_list_steps = list_flat sql
  where
    sql = "SELECT name FROM (SELECT name, COUNT(*) AS freq FROM build_steps where name IS NOT NULL GROUP BY name ORDER BY freq DESC, name ASC) foo WHERE name ILIKE CONCAT(?,'%') "


data SummaryStats = SummaryStats {
    _failed_builds      :: Int
  , _visited_builds     :: Int
  , _explained_failures :: Int
  , _timed_out_steps    :: Int
  , _steps_with_a_match :: Int
  } deriving Generic

instance ToJSON SummaryStats where
  toJSON = genericToJSON WebApi.dropUnderscore


api_summary_stats = do
  conn <- DbHelpers.get_connection

  [Only build_count] <- query_ conn "SELECT COUNT(*) FROM builds"
  [Only visited_count] <- query_ conn "SELECT COUNT(*) FROM build_steps"
  [Only explained_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE name IS NOT NULL"
  [Only timeout_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE is_timeout"
  [Only matched_steps_count] <- query_ conn "SELECT COUNT(*) FROM (SELECT build_step FROM public.matches GROUP BY build_step) x"

  return $ SummaryStats build_count visited_count explained_count timeout_count matched_steps_count






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
  toJSON = genericToJSON WebApi.dropUnderscore


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
  , _line_count   :: Int
  , _line_text    :: Text
  , _span_start   :: Int
  , _span_end     :: Int
  } deriving Generic

instance ToJSON PatternOccurrences where
  toJSON = genericToJSON WebApi.dropUnderscore


get_pattern_matches :: Int -> IO [PatternOccurrences]
get_pattern_matches pattern_id = do
  rows <- get_pattern_occurrence_rows pattern_id
  return $ map txform rows

  where
    txform (Builds.NewBuildNumber buildnum, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = PatternOccurrences buildnum stepname line_number line_count line_text start end


get_pattern_occurrence_rows :: Int -> IO [(Builds.BuildNumber, Text, Int, ScanPatterns.MatchDetails)]
get_pattern_occurrence_rows pattern_id = do

  conn <- DbHelpers.get_connection

  xs <- query conn "SELECT build_steps.build AS build_num, build_steps.name, line_number, line_count, line_text, span_start, span_end FROM (SELECT * FROM matches WHERE pattern = ?) foo JOIN build_steps ON build_steps.id = foo.build_step LEFT JOIN log_metadata ON log_metadata.step = build_steps.id ORDER BY build_num DESC" (Only pattern_id)

  inners <- forM xs $ \(buildnum, stepname, line_number, line_count, line_text, span_start, span_end) ->
    return $ (Builds.NewBuildNumber buildnum, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

  return inners

