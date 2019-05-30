{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad                        (forM)
import           Data.Aeson
import           Data.List                            (sort)
import           Data.List.Split                      (splitOn)
import           Data.Scientific                      (Scientific)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time                            (UTCTime)
import           Data.Time.Calendar                   (Day)
import           Data.Tuple                           (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           GHC.Generics
import           GHC.Int                              (Int64)
import qualified Safe

import qualified AuthStages
import qualified Breakages
import qualified Builds
import qualified BuildSteps
import qualified CommitBuilds
import qualified DbHelpers
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified ScanPatterns
import qualified ScanRecords
import qualified WebApi


split_agg_text :: String -> [String]
split_agg_text = filter (not . null) . splitOn ";"


construct_expression :: Bool -> Text -> Bool -> ScanPatterns.MatchExpression
construct_expression
    is_regex
    pattern_text
    is_nondeterministic = if is_regex
  then ScanPatterns.RegularExpression pattern_text is_nondeterministic
  else ScanPatterns.LiteralExpression pattern_text


wrap_pattern ::
     Int64
  -> Bool
  -> Text
  -> Bool
  -> Text
  -> [Text]
  -> [Text]
  -> Int
  -> Bool
  -> ScanPatterns.DbPattern
wrap_pattern pattern_id is_regex pattern_text is_nondeterministic description tags_list steps_list specificity is_retired =
  DbHelpers.WithId pattern_id inner_pattern
  where
    expression_obj = construct_expression is_regex pattern_text is_nondeterministic
    inner_pattern = ScanPatterns.NewPattern expression_obj description tags_list steps_list specificity is_retired


get_patterns :: Connection -> IO [ScanPatterns.DbPattern]
get_patterns conn = do

  patterns_rows <- query_ conn patterns_sql

  forM patterns_rows $ \(pattern_id, is_regex, pattern_text, has_nondeterministic_values, description, specificity, is_retired) -> do
    tags_rows <- query conn tags_sql (Only pattern_id)
    tags_list <- forM tags_rows $ \(Only tag_text) -> return tag_text

    steps_rows <- query conn applicable_steps_sql (Only pattern_id)
    steps_list <- forM steps_rows $ \(Only step_text) -> return step_text

    return $ wrap_pattern pattern_id is_regex pattern_text has_nondeterministic_values description tags_list steps_list specificity is_retired

  where
    patterns_sql = "SELECT id, regex, expression, has_nondeterministic_values, description, specificity, is_retired FROM patterns ORDER BY description;"

    tags_sql = "SELECT tag FROM pattern_tags WHERE pattern = ?;"
    applicable_steps_sql = "SELECT step_name FROM pattern_step_applicability WHERE pattern = ?;"


get_unvisited_build_ids :: Connection -> Maybe Int -> IO [Builds.BuildNumber]
get_unvisited_build_ids conn maybe_limit = do
  rows <- case maybe_limit of
    Just limit -> query conn sql (Only limit)
    Nothing    -> query_ conn unlimited_sql
  return $ map (\(Only num) -> Builds.NewBuildNumber num) rows
  where
    sql = "SELECT build_num FROM unvisited_builds ORDER BY build_NUM DESC LIMIT ?;"
    unlimited_sql = "SELECT build_num FROM unvisited_builds ORDER BY build_NUM DESC;"


get_revisitable_builds :: Connection -> IO [(Builds.BuildStepId, Text, Builds.BuildNumber, [Int64])]
get_revisitable_builds conn = do
  rows <- query_ conn sql
  return $ map f rows
  where
    f (delimited_pattern_ids, step_id, step_name, build_id) =
      ( Builds.NewBuildStepId step_id
      , step_name
      , Builds.NewBuildNumber build_id
      , map read $ splitOn ";" delimited_pattern_ids
      )

    sql = "SELECT string_agg((patterns.id)::text, ';'), MAX(step_id) AS step_id, MAX(name) AS step_name, build_num FROM (SELECT COALESCE(scanned_patterns.newest_pattern, -1) AS latest_pattern, build_steps.build AS build_num, build_steps.name, build_steps.id AS step_id FROM build_steps LEFT JOIN scanned_patterns ON scanned_patterns.build = build_steps.build WHERE build_steps.name IS NOT NULL AND NOT build_steps.is_timeout) foo, patterns WHERE patterns.id > latest_pattern GROUP BY build_num;"


get_latest_pattern_id :: Connection -> IO ScanRecords.PatternId
get_latest_pattern_id conn = do
  [Only pattern_id] <- query_ conn sql
  return $ ScanRecords.NewPatternId pattern_id
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1;"


-- XXX NOT USED
query_builds :: DbHelpers.DbConnectionData -> IO [Builds.Build]
query_builds conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (buildnum, vcs_rev, queuedat, jobname, branch) =
      Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queuedat jobname branch

    sql = "SELECT build_num, vcs_revision, queued_at, job_name, branch FROM builds;"


api_line_count_histogram :: DbHelpers.DbConnectionData -> IO [(Text, Int)]
api_line_count_histogram conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ map (swap . f) xs
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)
    sql = "select count(*) as qty, pow(10, floor(ln(line_count) / ln(10)))::numeric::integer as bin from log_metadata WHERE line_count > 0 group by bin ORDER BY bin ASC;"


api_jobs :: DbHelpers.DbConnectionData -> IO (WebApi.ApiResponse WebApi.JobApiRecord)
api_jobs conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ WebApi.ApiResponse $ map f xs
  where
    f (jobname, freq) = WebApi.JobApiRecord jobname [freq]
    sql = "SELECT job_name, freq FROM job_failure_frequencies;"


api_step :: DbHelpers.DbConnectionData -> IO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
api_step conn_data = do
  conn <- DbHelpers.get_connection conn_data

  xs <- query_ conn sql
  let inners = map (\(stepname, freq) -> WebApi.PieSliceApiRecord stepname freq) xs
  return $ WebApi.ApiResponse inners

  where
    sql = "SELECT name, COUNT(*) AS freq FROM build_steps WHERE name IS NOT NULL GROUP BY name ORDER BY freq DESC;"


-- | Note that Highcharts expects the dates to be in ascending order
api_failed_commits_by_day :: DbHelpers.DbConnectionData -> IO (WebApi.ApiResponse (Day, Int))
api_failed_commits_by_day conn_data = do
  conn <- DbHelpers.get_connection conn_data
  WebApi.ApiResponse <$> query_ conn sql
  where
    sql = "SELECT queued_at::date AS date, COUNT(*) FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM builds GROUP BY vcs_revision) foo GROUP BY date ORDER BY date ASC;"


get_flaky_pattern_ids :: Connection -> IO (Set Int64)
get_flaky_pattern_ids conn = do
  xs <- query_ conn sql
  return $ Set.fromList $ map (\(Only x) -> x) xs
  where
    sql = "SELECT id FROM flaky_patterns_augmented;"


list_builds :: Query -> DbHelpers.DbConnectionData -> IO [WebApi.BuildBranchRecord]
list_builds sql conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (buildnum, branch) = WebApi.BuildBranchRecord (Builds.NewBuildNumber buildnum) branch


api_unmatched_builds :: DbHelpers.DbConnectionData -> IO [WebApi.BuildBranchRecord]
api_unmatched_builds = list_builds sql
  where
    sql = "SELECT build, branch FROM unattributed_failed_builds;"


api_idiopathic_builds :: DbHelpers.DbConnectionData -> IO [WebApi.BuildBranchRecord]
api_idiopathic_builds = list_builds sql
  where
    sql = "SELECT build, branch FROM idiopathic_build_failures;"


api_random_scannable_build :: DbHelpers.DbConnectionData -> IO WebApi.BuildNumberRecord
api_random_scannable_build conn_data = do
  conn <- DbHelpers.get_connection conn_data
  [Only buildnum] <- query_ conn sql
  return $ WebApi.BuildNumberRecord $ Builds.NewBuildNumber buildnum
  where
    sql = "SELECT build_num FROM scannable_build_steps OFFSET floor(random()*(SELECT COUNT(*) FROM scannable_build_steps)) LIMIT 1;"


-- | Obtains the console log from database
read_log :: Connection -> Builds.BuildNumber -> IO (Maybe Text)
read_log conn (Builds.NewBuildNumber build_num) = do
  result <- query conn sql $ Only build_num
  return $ (\(Only log_text) -> log_text) <$> Safe.headMay result
  where
    sql = "SELECT log_metadata.content FROM log_metadata JOIN builds_join_steps ON log_metadata.step = builds_join_steps.step_id WHERE builds_join_steps.build_num = ? LIMIT 1;"


list_flat :: FromField a => Query -> DbHelpers.DbConnectionData -> Text -> IO [a]
list_flat sql conn_data t = do
  conn <- DbHelpers.get_connection conn_data
  inners <- query conn sql $ Only t
  return $ map (\(Only x) -> x) inners


api_autocomplete_tags :: DbHelpers.DbConnectionData -> Text -> IO [Text]
api_autocomplete_tags = list_flat sql
  where
    sql = "SELECT tag FROM (SELECT tag, COUNT(*) AS freq FROM pattern_tags GROUP BY tag ORDER BY freq DESC, tag ASC) foo WHERE tag ILIKE CONCAT(?,'%');"


api_autocomplete_steps :: DbHelpers.DbConnectionData -> Text -> IO [Text]
api_autocomplete_steps = list_flat sql
  where
    sql = "SELECT name FROM (SELECT name, COUNT(*) AS freq FROM build_steps where name IS NOT NULL GROUP BY name ORDER BY freq DESC, name ASC) foo WHERE name ILIKE CONCAT(?,'%');"


api_autocomplete_branches :: DbHelpers.DbConnectionData -> Text -> IO [Text]
api_autocomplete_branches = list_flat sql
  where
    sql = "SELECT branch FROM builds WHERE branch ILIKE CONCAT(?,'%') GROUP BY branch ORDER BY COUNT(*) DESC;"


-- Not used yet
api_list_branches :: DbHelpers.DbConnectionData -> IO [Text]
api_list_branches conn_data = do
  conn <- DbHelpers.get_connection conn_data
  inners <- query_ conn sql
  return $ map (\(Only x) -> x) inners
  where
    sql = "SELECT branch, COUNT(*) AS count FROM builds GROUP BY branch ORDER BY count DESC;"


get_revision_builds :: DbHelpers.DbConnectionData -> GitRev.GitSha1 -> IO [CommitBuilds.CommitBuild]
get_revision_builds conn_data git_revision = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only $ GitRev.sha1 git_revision
  return $ map f xs

  where
    f (step_name, buildnum, vcs_rev, queuedat, jobname, branch, pattern, line_number, line_count, line_text, span_start, span_end, specificity) =
      CommitBuilds.NewCommitBuild
        (Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queuedat jobname branch)
        (MatchOccurrences.MatchOccurrencesForBuild step_name pattern line_number line_count line_text span_start span_end specificity)

    sql = "SELECT step_name, build, vcs_revision, queued_at, job_name, branch, pattern_id, line_number, line_count, line_text, span_start, span_end, patterns_augmented.specificity FROM best_pattern_match_augmented_builds JOIN patterns_augmented on best_pattern_match_augmented_builds.pattern_id = patterns_augmented.id WHERE vcs_revision = ?;"


data CommitInfo = NewCommitInfo {
    _failed_build_count :: Int
  } deriving Generic

instance ToJSON CommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


count_revision_builds :: DbHelpers.DbConnectionData -> GitRev.GitSha1 -> IO CommitInfo
count_revision_builds conn_data git_revision = do
  conn <- DbHelpers.get_connection conn_data
  [Only x] <- query conn sql (Only $ GitRev.sha1 git_revision)
  return $ NewCommitInfo x
  where
    sql = "SELECT COUNT(*) FROM best_pattern_match_augmented_builds WHERE vcs_revision = ?;"


-- | NOTE: Some of these values can be derived from the others.
-- We query for them all as a sanity check.
data SummaryStats = SummaryStats {
    _failed_builds              :: Int
  , _visited_builds             :: Int
  , _explained_failures         :: Int
  , _timed_out_steps            :: Int
  , _steps_with_a_match         :: Int
  , _unattributed_failed_builds :: Int
  , _idiopathic_build_failures  :: Int
  } deriving Generic

instance ToJSON SummaryStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


api_summary_stats conn_data = do
  conn <- DbHelpers.get_connection conn_data

  [Only build_count] <- query_ conn "SELECT COUNT(*) FROM builds"
  [Only visited_count] <- query_ conn "SELECT COUNT(*) FROM build_steps"
  [Only explained_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE name IS NOT NULL"
  [Only timeout_count] <- query_ conn "SELECT COUNT(*) FROM build_steps WHERE is_timeout"
  [Only matched_steps_count] <- query_ conn "SELECT COUNT(*) FROM (SELECT build_step FROM public.matches GROUP BY build_step) x"
  [Only unattributed_failed_builds] <- query_ conn "SELECT COUNT(*) FROM unattributed_failed_builds"
  [Only idiopathic_build_failures] <- query_ conn "SELECT COUNT(*) FROM idiopathic_build_failures"
  return $ SummaryStats build_count visited_count explained_count timeout_count matched_steps_count unattributed_failed_builds idiopathic_build_failures


data PatternRecord = PatternRecord {
    _id              :: Int64
  , _is_regex        :: Bool
  , _pattern         :: Text
  , _description     :: Text
  , _frequency       :: Int
  , _last            :: Maybe UTCTime
  , _earliest        :: Maybe UTCTime
  , _tags            :: [String]
  , _steps           :: [String]
  , _specificity     :: Int
  , _percent_scanned :: Scientific
  } deriving Generic

instance ToJSON PatternRecord where
  toJSON = genericToJSON JsonUtils.dropUnderscore


make_pattern_records =
  map $ \(a, b, c, d, e, f, g, h, i, j, k) ->
    PatternRecord a b c d e f g (split_agg_text h) (split_agg_text i) j k


api_single_pattern :: DbHelpers.DbConnectionData -> Int ->  IO [PatternRecord]
api_single_pattern conn_data pattern_id = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ make_pattern_records xs
  where
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary WHERE id = ?;"


api_patterns :: DbHelpers.DbConnectionData -> IO [PatternRecord]
api_patterns conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ make_pattern_records xs
  where
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary ORDER BY matching_build_count DESC;"


-- | For the purpose of database upgrades
dump_patterns :: DbHelpers.DbConnectionData -> IO [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
dump_patterns conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ map f xs

  where
    f (author, created, pattern_id, is_regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired) =
      DbHelpers.WithAuthorship author created $ wrap_pattern pattern_id is_regex expression has_nondeterministic_values description
        (sort $ map T.pack $ split_agg_text tags)
        (sort $ map T.pack $ split_agg_text steps)
        specificity
        is_retired

    sql = "SELECT author, created, id, regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired FROM patterns_augmented ORDER BY id;"


-- | Note that this SQL is from decomposing the "pattern_frequency_summary" and "aggregated_build_matches" view
-- to parameterize the latter by branch.
api_patterns_branch_filtered :: DbHelpers.DbConnectionData -> [Text] -> IO [PatternRecord]
api_patterns_branch_filtered conn_data branches = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only $ In branches
  return $ make_pattern_records xs

  where
    sql = "SELECT patterns_augmented.id, patterns_augmented.regex, patterns_augmented.expression, patterns_augmented.description, COALESCE(aggregated_build_matches.matching_build_count, 0::int) AS matching_build_count, aggregated_build_matches.most_recent, aggregated_build_matches.earliest, patterns_augmented.tags, patterns_augmented.steps, patterns_augmented.specificity, CAST((patterns_augmented.scanned_count * 100 / patterns_augmented.total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM patterns_augmented LEFT JOIN ( SELECT best_pattern_match_for_builds.pattern_id AS pat, count(best_pattern_match_for_builds.build) AS matching_build_count, max(builds.queued_at) AS most_recent, min(builds.queued_at) AS earliest FROM best_pattern_match_for_builds JOIN builds ON builds.build_num = best_pattern_match_for_builds.build WHERE builds.branch IN ? GROUP BY best_pattern_match_for_builds.pattern_id) aggregated_build_matches ON patterns_augmented.id = aggregated_build_matches.pat ORDER BY matching_build_count DESC;"


data PatternOccurrences = PatternOccurrences {
    _build_number :: Int64
  , _vsc_revision :: Text
  , _queued_at    :: UTCTime
  , _job_name     :: Text
  , _branch       :: Text
  , _build_step   :: Text
  , _line_number  :: Int
  , _line_count   :: Int
  , _line_text    :: Text
  , _span_start   :: Int
  , _span_end     :: Int
  } deriving Generic

instance ToJSON PatternOccurrences where
  toJSON = genericToJSON JsonUtils.dropUnderscore


get_build_pattern_matches :: DbHelpers.DbConnectionData -> Int -> IO [MatchOccurrences.MatchOccurrencesForBuild]
get_build_pattern_matches conn_data build_id = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id
  return $ map f xs

  where
    f (step_name, pattern, line_number, line_count, line_text, span_start, span_end, specificity) =
      MatchOccurrences.MatchOccurrencesForBuild
        step_name pattern line_number line_count line_text span_start span_end specificity

    sql = "SELECT step_name, pattern, line_number, line_count, line_text, span_start, span_end, specificity FROM matches_with_log_metadata JOIN build_steps ON matches_with_log_metadata.build_step = build_steps.id JOIN patterns_augmented ON patterns_augmented.id = matches_with_log_metadata.pattern WHERE matches_with_log_metadata.build_num = ? ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"


pattern_occurence_txform = txform . f
  where
    -- TODO consolidate this transformation with "get_pattern_matches"
    f (buildnum, stepname, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_revision queued_at job_name branch, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

    txform ((Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queued_at job_name branch), stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = PatternOccurrences buildnum vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_best_pattern_matches :: DbHelpers.DbConnectionData -> Int -> IO [PatternOccurrences]
get_best_pattern_matches conn_data pattern_id = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ map pattern_occurence_txform xs

  where
    sql = "SELECT build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE pattern_id = ?;"


-- | This should produce one or zero results.
-- We use a list instead of a Maybe so that
-- the javascript table renderer code can be reused
-- for multi-item lists.
get_best_build_match :: DbHelpers.DbConnectionData -> Int -> IO [PatternOccurrences]
get_best_build_match conn_data build_id = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id
  return $ map pattern_occurence_txform xs

  where
    sql = "SELECT build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE build = ?;"


get_build_info :: DbHelpers.DbConnectionData -> Int -> IO BuildSteps.BuildStep
get_build_info conn_data build_id = do

  conn <- DbHelpers.get_connection conn_data
  [(step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, maybe_implicated_revision, maybe_is_broken, maybe_notes, maybe_reporter)] <- query conn sql $ Only build_id

  let build_obj = Builds.NewBuild (Builds.NewBuildNumber build_num) vcs_revision queued_at job_name branch
      maybe_breakage_obj = do
        is_broken <- maybe_is_broken
        notes <- maybe_notes
        reporter <- maybe_reporter
        return $ Breakages.NewBreakageReport vcs_revision maybe_implicated_revision is_broken notes $ AuthStages.Username reporter

  return $ BuildSteps.NewBuildStep step_name (Builds.NewBuildStepId step_id) build_obj maybe_breakage_obj
  where
    sql = "SELECT step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, implicated_revision, is_broken, breakage_notes, reporter FROM builds_with_reports where build_num = ?;"


get_pattern_matches :: DbHelpers.DbConnectionData -> Int -> IO [PatternOccurrences]
get_pattern_matches conn_data pattern_id = do
  rows <- get_pattern_occurrence_rows conn_data pattern_id
  return $ map f rows

  where
    f ((Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queued_at job_name branch), stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) =
      PatternOccurrences buildnum vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_pattern_occurrence_rows :: DbHelpers.DbConnectionData -> Int -> IO [(Builds.Build, Text, Int, ScanPatterns.MatchDetails)]
get_pattern_occurrence_rows conn_data pattern_id = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ map f xs

  where
    f (buildnum, stepname, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_revision queued_at job_name branch, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

--    sql = "SELECT builds.build_num, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM (SELECT build_steps.build AS build_num, build_steps.name AS step_name, line_number, COALESCE(line_count, 0) AS line_count, line_text, span_start, span_end FROM (SELECT * FROM matches WHERE pattern = ?) foo JOIN build_steps ON build_steps.id = foo.build_step LEFT JOIN log_metadata ON log_metadata.step = build_steps.id ORDER BY build_num DESC) bar JOIN builds ON builds.build_num = bar.build_num;"
    -- simplified:
    sql = "SELECT builds.build_num, step_name, line_number, line_count, line_text, span_start, span_end, builds.vcs_revision, queued_at, job_name, branch FROM matches_with_log_metadata JOIN builds ON matches_with_log_metadata.build_num = builds.build_num WHERE pattern = ?;"
