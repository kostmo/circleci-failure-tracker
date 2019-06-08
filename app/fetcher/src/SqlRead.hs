{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad                        (forM)
import           Data.Aeson
import           Data.Either.Utils                    (maybeToEither)
import           Data.List                            (sort, sortOn)
import           Data.List.Split                      (splitOn)
import qualified Data.Maybe                           as Maybe
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
import qualified ScanUtils
import qualified StoredBreakageReports
import qualified WebApi


testFailurePatternId :: ScanPatterns.PatternId
testFailurePatternId = ScanPatterns.PatternId 302


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
  -> Maybe Int
  -> ScanPatterns.DbPattern
wrap_pattern pattern_id is_regex pattern_text is_nondeterministic description tags_list steps_list specificity is_retired maybe_lines_from_end =
  DbHelpers.WithId pattern_id inner_pattern
  where
    expression_obj = construct_expression is_regex pattern_text is_nondeterministic
    inner_pattern = ScanPatterns.NewPattern expression_obj description tags_list steps_list specificity is_retired maybe_lines_from_end


get_patterns :: Connection -> IO [ScanPatterns.DbPattern]
get_patterns conn = do

  patterns_rows <- query_ conn patterns_sql

  forM patterns_rows $ \(pattern_id, is_regex, pattern_text, has_nondeterministic_values, description, specificity, is_retired, lines_from_end) -> do
    tags_rows <- query conn tags_sql $ Only pattern_id
    tags_list <- forM tags_rows $ \(Only tag_text) -> return tag_text

    steps_rows <- query conn applicable_steps_sql $ Only pattern_id
    steps_list <- forM steps_rows $ \(Only step_text) -> return step_text

    return $ wrap_pattern pattern_id is_regex pattern_text has_nondeterministic_values description tags_list steps_list specificity is_retired lines_from_end

  where
    patterns_sql = "SELECT id, regex, expression, has_nondeterministic_values, description, specificity, is_retired, lines_from_end FROM patterns ORDER BY description;"

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


get_latest_pattern_id :: Connection -> IO ScanPatterns.PatternId
get_latest_pattern_id conn = do
  [Only pattern_id] <- query_ conn sql
  return $ ScanPatterns.PatternId pattern_id
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1;"


data PostedStatus = PostedStatus {
    _sha1        :: Text
  , _description :: Text
  , _state       :: Text
  , _created_at  :: UTCTime
  } deriving Generic

instance ToJSON PostedStatus where
  toJSON = genericToJSON JsonUtils.dropUnderscore


api_posted_statuses :: DbHelpers.DbConnectionData -> IO [PostedStatus]
api_posted_statuses conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (sha1, description, state, created_at) = PostedStatus sha1 description state created_at
    sql = "SELECT sha1, description, state, created_at FROM created_github_statuses ORDER BY created_at DESC LIMIT 40;"


data PatternsTimelinePoint = PatternsTimelinePoint {
    _pattern_id :: Int64
  , _count      :: Int
  , _week       :: UTCTime
  } deriving Generic

instance ToJSON PatternsTimelinePoint where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data PatternsTimeline = PatternsTimeline {
    _patterns :: [PatternRecord]
  , _points   :: [PatternsTimelinePoint]
  } deriving Generic

instance ToJSON PatternsTimeline where
  toJSON = genericToJSON JsonUtils.dropUnderscore


api_pattern_occurrence_timeline conn_data = do
  conn <- DbHelpers.get_connection conn_data
  points <- map f <$> query_ conn timeline_sql
  patterns <- api_patterns conn_data
  let filtered_patterns = sortOn (negate . _frequency) $ filter ((> 0) . _frequency) patterns
  return $ PatternsTimeline filtered_patterns points
  where
    f (pattern_id, week, count) = PatternsTimelinePoint pattern_id count week

    timeline_sql = "SELECT pattern_id, date_trunc('week', queued_at) AS week, COUNT(*) AS occurrences FROM best_pattern_match_augmented_builds WHERE branch IN (SELECT branch FROM presumed_stable_branches) GROUP BY pattern_id, week"


data TestFailure = TestFailure {
    _sha1       :: Text
  , _test_name  :: Text
  , _build_date :: UTCTime
  } deriving Generic

instance ToJSON TestFailure where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | This uses capture groups of a specifically-crafted regex
-- to identify the name of the failing test
api_test_failures :: DbHelpers.DbConnectionData -> IO (Either Text [TestFailure])
api_test_failures conn_data = do
  patterns_singleton <- api_single_pattern conn_data testFailurePatternId

  case Safe.headMay patterns_singleton of
    Nothing -> return $ Left "Could not find Test Failure pattern"
    Just test_failure_pattern -> do
      pattern_occurrences <- get_best_pattern_matches_whitelisted_branches conn_data testFailurePatternId
      return $ Right $ Maybe.mapMaybe (repackage test_failure_pattern) pattern_occurrences

  where

    repackage test_failure_pattern pattern_occurrence = do
      maybe_first_match <- maybe_first_match_group
      return $ TestFailure
          (_vcs_revision pattern_occurrence)
          (T.pack maybe_first_match)
          (_queued_at pattern_occurrence)
      where
        start_idx = _span_start pattern_occurrence
        end_idx = _span_end pattern_occurrence
        span_length = end_idx - start_idx
        extracted_chunk = T.take span_length $ T.drop start_idx $ _line_text pattern_occurrence

        pattern_text = _pattern test_failure_pattern
        maybe_first_match_group = ScanUtils.get_first_match_group extracted_chunk pattern_text


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


api_byte_count_histogram :: DbHelpers.DbConnectionData -> IO [(Text, Int)]
api_byte_count_histogram conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ map (swap . f) xs
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)
    sql = "select count(*) as qty, pow(10, floor(ln(byte_count) / ln(10)))::numeric::integer as bin from log_metadata WHERE byte_count > 0 group by bin ORDER BY bin ASC;"


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


-- | Note that Highcharts expects the dates to be in ascending order
api_status_postings_by_day :: DbHelpers.DbConnectionData -> IO (WebApi.ApiResponse (Day, Int))
api_status_postings_by_day conn_data = do
  conn <- DbHelpers.get_connection conn_data
  WebApi.ApiResponse <$> query_ conn sql
  where
    sql = "SELECT created_at::date AS date, COUNT(*) FROM created_github_statuses GROUP BY date ORDER BY date ASC;"


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


api_commit_breakage_reports :: DbHelpers.DbConnectionData -> Text -> IO [StoredBreakageReports.BreakageReport]
api_commit_breakage_reports conn_data sha1 = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query conn sql (Only sha1)
  where
    f (build_num, step_name, job_name, is_broken, reporter, report_timestamp, breakage_notes, implicated_revision) = StoredBreakageReports.BreakageReport (Builds.NewBuildNumber build_num) step_name job_name is_broken (AuthStages.Username reporter) report_timestamp breakage_notes implicated_revision
    sql = "SELECT build_num, step_name, job_name, is_broken, reporter, report_timestamp, breakage_notes, implicated_revision FROM builds_with_reports WHERE vcs_revision = ? AND is_broken IS NOT NULL"


api_unmatched_commit_builds :: DbHelpers.DbConnectionData -> Text -> IO [WebApi.UnmatchedBuild]
api_unmatched_commit_builds conn_data sha1 = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, is_broken) = WebApi.UnmatchedBuild (Builds.NewBuildNumber build) step_name queued_at job_name branch is_broken
    sql = "SELECT build, step_name, queued_at, job_name, unattributed_failed_builds.branch, is_broken FROM unattributed_failed_builds LEFT JOIN builds_with_reports ON unattributed_failed_builds.build = builds_with_reports.build_num WHERE vcs_revision = ?"


api_idiopathic_builds :: DbHelpers.DbConnectionData -> IO [WebApi.BuildBranchRecord]
api_idiopathic_builds = list_builds sql
  where
    sql = "SELECT build, branch FROM idiopathic_build_failures;"


api_idiopathic_commit_builds :: DbHelpers.DbConnectionData -> Text -> IO [WebApi.UnmatchedBuild]
api_idiopathic_commit_builds conn_data sha1 = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, is_broken) = WebApi.UnmatchedBuild (Builds.NewBuildNumber build) step_name queued_at job_name branch is_broken
    sql = "SELECT build, step_name, queued_at, job_name, idiopathic_build_failures.branch, is_broken FROM idiopathic_build_failures LEFT JOIN builds_with_reports ON idiopathic_build_failures.build = builds_with_reports.build_num WHERE vcs_revision = ?"


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
    f (step_name, buildnum, vcs_rev, queuedat, jobname, branch, pattern, line_number, line_count, line_text, span_start, span_end, specificity, maybe_is_broken, maybe_reporter, maybe_report_timestamp) =
      CommitBuilds.NewCommitBuild
        build_obj
        match_obj
        (maybe_breakage_report maybe_is_broken  maybe_reporter maybe_report_timestamp)
      where
        build_obj = Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_rev queuedat jobname branch
        match_obj = MatchOccurrences.MatchOccurrencesForBuild step_name (ScanPatterns.PatternId pattern) line_number line_count line_text span_start span_end specificity

        maybe_breakage_report :: Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe CommitBuilds.StoredBreakageReport
        maybe_breakage_report x y z = CommitBuilds.StoredBreakageReport <$> x <*> (AuthStages.Username <$> y) <*> z

    sql = "SELECT step_name, build, vcs_revision, queued_at, job_name, branch, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, is_broken, reporter, report_timestamp FROM best_pattern_match_augmented_builds WHERE vcs_revision = ?;"


data CommitInfo = NewCommitInfo {
    _failed_build_count  :: Int
  , _matched_build_count :: Int
  , _code_breakage_count :: Int
  } deriving Generic

instance ToJSON CommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


count_revision_builds :: DbHelpers.DbConnectionData -> GitRev.GitSha1 -> IO CommitInfo
count_revision_builds conn_data git_revision = do
  conn <- DbHelpers.get_connection conn_data
  [Only failed_count] <- query conn failed_count_sql only_commit
  [Only matched_count] <- query conn matched_count_sql only_commit
  [Only reported_count] <- query conn reported_broken_count_sql only_commit

  return $ NewCommitInfo failed_count matched_count reported_count
  where
    only_commit = Only $ GitRev.sha1 git_revision

    failed_count_sql = "SELECT COUNT(*) FROM builds WHERE vcs_revision = ?;"
    matched_count_sql = "SELECT COUNT(*) FROM best_pattern_match_augmented_builds WHERE vcs_revision = ?;"
    reported_broken_count_sql = "SELECT COUNT(*) FROM builds_with_reports WHERE vcs_revision = ? AND is_broken;"


data ScanTestResponse = ScanTestResponse {
    _total_line_count :: Int
  , _matches          :: [ScanPatterns.ScanMatch]
  } deriving Generic

instance ToJSON ScanTestResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


api_new_pattern_test ::
     DbHelpers.DbConnectionData
  -> Builds.BuildNumber
  -> ScanPatterns.Pattern
  -> IO (Either String ScanTestResponse)
api_new_pattern_test conn_data build_number@(Builds.NewBuildNumber buildnum) new_pattern = do

  conn <- DbHelpers.get_connection conn_data

  -- TODO consolidate with Scanning.scan_log
  maybe_console_log <- SqlRead.read_log conn build_number

  return $ case maybe_console_log of
            Just console_log -> Right $ ScanTestResponse (length $ T.lines console_log) $
              Maybe.mapMaybe apply_pattern $ zip [0::Int ..] $ map T.stripEnd $ T.lines console_log
            Nothing -> Left $ "No log found for build number " ++ show buildnum
  where
    apply_pattern :: (Int, Text) -> Maybe ScanPatterns.ScanMatch
    apply_pattern line_tuple = ScanUtils.apply_single_pattern line_tuple $ DbHelpers.WithId 0 new_pattern


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


-- | Returns zero or one pattern.
api_single_pattern :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId ->  IO [PatternRecord]
api_single_pattern conn_data (ScanPatterns.PatternId pattern_id) = do
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
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary ORDER BY most_recent DESC NULLS LAST;"


-- | For the purpose of database upgrades
dump_presumed_stable_branches :: DbHelpers.DbConnectionData -> IO [Text]
dump_presumed_stable_branches conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ map (\(Only x) -> x) xs
  where
    sql = "SELECT branch FROM presumed_stable_branches ORDER BY branch;"


-- | For the purpose of database upgrades
dump_patterns :: DbHelpers.DbConnectionData -> IO [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
dump_patterns conn_data = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query_ conn sql
  return $ map f xs

  where
    f (author, created, pattern_id, is_regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end) =
      DbHelpers.WithAuthorship author created $ wrap_pattern pattern_id is_regex expression has_nondeterministic_values description
        (sort $ map T.pack $ split_agg_text tags)
        (sort $ map T.pack $ split_agg_text steps)
        specificity
        is_retired
        lines_from_end

    sql = "SELECT author, created, id, regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end FROM patterns_augmented ORDER BY id;"


-- | Note that this SQL is from decomposing the "pattern_frequency_summary" and "aggregated_build_matches" view
-- to parameterize the latter by branch.
api_patterns_branch_filtered :: DbHelpers.DbConnectionData -> [Text] -> IO [PatternRecord]
api_patterns_branch_filtered conn_data branches = do
  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only $ In branches
  return $ make_pattern_records xs

  where
    sql = "SELECT patterns_augmented.id, patterns_augmented.regex, patterns_augmented.expression, patterns_augmented.description, COALESCE(aggregated_build_matches.matching_build_count, 0::int) AS matching_build_count, aggregated_build_matches.most_recent, aggregated_build_matches.earliest, patterns_augmented.tags, patterns_augmented.steps, patterns_augmented.specificity, CAST((patterns_augmented.scanned_count * 100 / patterns_augmented.total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM patterns_augmented LEFT JOIN ( SELECT best_pattern_match_for_builds.pattern_id AS pat, count(best_pattern_match_for_builds.build) AS matching_build_count, max(builds.queued_at) AS most_recent, min(builds.queued_at) AS earliest FROM best_pattern_match_for_builds JOIN builds ON builds.build_num = best_pattern_match_for_builds.build WHERE builds.branch IN ? GROUP BY best_pattern_match_for_builds.pattern_id) aggregated_build_matches ON patterns_augmented.id = aggregated_build_matches.pat ORDER BY matching_build_count DESC;"


data PatternOccurrence = PatternOccurrence {
    _build_number :: Builds.BuildNumber
  , _pattern_id   :: ScanPatterns.PatternId
  , _vcs_revision :: Text
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

instance ToJSON PatternOccurrence where
  toJSON = genericToJSON JsonUtils.dropUnderscore


get_build_pattern_matches :: DbHelpers.DbConnectionData -> Builds.BuildNumber -> IO [MatchOccurrences.MatchOccurrencesForBuild]
get_build_pattern_matches conn_data (Builds.NewBuildNumber build_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id
  return $ map f xs

  where
    f (step_name, pattern, line_number, line_count, line_text, span_start, span_end, specificity) =
      MatchOccurrences.MatchOccurrencesForBuild
        step_name (ScanPatterns.PatternId pattern) line_number line_count line_text span_start span_end specificity

    sql = "SELECT step_name, pattern, line_number, line_count, line_text, span_start, span_end, specificity FROM matches_with_log_metadata JOIN build_steps ON matches_with_log_metadata.build_step = build_steps.id JOIN patterns_augmented ON patterns_augmented.id = matches_with_log_metadata.pattern WHERE matches_with_log_metadata.build_num = ? ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"


data StorageStats = StorageStats {
    _total_lines :: Integer
  , _total_bytes :: Integer
  , _log_count   :: Integer
  } deriving Generic

instance ToJSON StorageStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


api_storage_stats :: DbHelpers.DbConnectionData -> IO StorageStats
api_storage_stats conn_data = do
  conn <- DbHelpers.get_connection conn_data
  [(a, b, c)] <- query_ conn sql
  return $ StorageStats a b c
  where
    sql = "SELECT SUM(line_count) AS total_lines, SUM(byte_count) AS total_bytes, COUNT(*) log_count FROM log_metadata"


pattern_occurence_txform pattern_id = txform . f
  where
    -- TODO consolidate this transformation with "get_pattern_matches"
    f (buildnum, stepname, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_revision queued_at job_name branch, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

    txform ((Builds.NewBuild buildnum vcs_rev queued_at job_name branch), stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = PatternOccurrence buildnum pattern_id vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_best_pattern_matches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_best_pattern_matches conn_data pat@(ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ map (pattern_occurence_txform pat) xs

  where
    sql = "SELECT build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE pattern_id = ?;"


get_best_pattern_matches_whitelisted_branches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_best_pattern_matches_whitelisted_branches conn_data pat@(ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data

  xs <- query conn sql $ Only pattern_id
  return $ map (pattern_occurence_txform pat) xs

  where
    sql = "SELECT build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE pattern_id = ? AND branch IN (SELECT branch from presumed_stable_branches);"


get_posted_github_status :: DbHelpers.DbConnectionData -> DbHelpers.OwnerAndRepo -> Text -> IO (Maybe (Text, Text))
get_posted_github_status conn_data (DbHelpers.OwnerAndRepo project repo) sha1 = do

  conn <- DbHelpers.get_connection conn_data

  xs <- query conn sql (sha1, project, repo)
  return $ Safe.headMay xs

  where
    sql = "SELECT state, description FROM created_github_statuses WHERE sha1 = ? AND project = ? AND repo = ? ORDER BY id DESC LIMIT 1;"


-- | This should produce one or zero results.
-- We use a list instead of a Maybe so that
-- the javascript table renderer code can be reused
-- for multi-item lists.
get_best_build_match :: DbHelpers.DbConnectionData -> Builds.BuildNumber -> IO [PatternOccurrence]
get_best_build_match conn_data (Builds.NewBuildNumber build_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id
  return $ map f xs

  where
    f (pattern_id, build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) = pattern_occurence_txform (ScanPatterns.PatternId pattern_id) (build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch)

    sql = "SELECT pattern_id, build, step_name, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE build = ?;"


data LogContext = LogContext {
    _match_info :: PatternOccurrence
  , _log_lines  :: [(Int, Text)]
  } deriving Generic

instance ToJSON LogContext where
  toJSON = genericToJSON JsonUtils.dropUnderscore


log_context_func connection_data build_id context_linecount = do
  conn <- DbHelpers.get_connection connection_data
  maybe_log <- SqlRead.read_log conn build_id
  maybe_best_build_match <- Safe.headMay <$> SqlRead.get_best_build_match connection_data build_id

  let maybe_result = do
        console_log <- maybe_log
        let log_lines = T.lines console_log
        best_build_match <- maybe_best_build_match
        let match_line = SqlRead._line_number best_build_match
            first_context_line = max 0 $ match_line - context_linecount

            tuples = zip [first_context_line..] $ take (2*context_linecount + 1) $ drop first_context_line log_lines
        return $ LogContext best_build_match tuples

  return $ maybeToEither "log not in database" maybe_result


data SingleBuildInfo = SingleBuildInfo {
    _multi_match_count :: Int
  , _build_info        :: BuildSteps.BuildStep
  } deriving Generic

instance ToJSON SingleBuildInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


get_build_info :: DbHelpers.DbConnectionData -> Builds.BuildNumber -> IO (Maybe SingleBuildInfo)
get_build_info conn_data build@(Builds.NewBuildNumber build_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id

  -- TODO Replace this with SQL COUNT()
  matches <- get_build_pattern_matches conn_data build

  return $ f (length matches) <$> Safe.headMay xs
  where
    f multi_match_count (step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, maybe_implicated_revision, maybe_is_broken, maybe_notes, maybe_reporter) = SingleBuildInfo multi_match_count step_container
      where
        step_container = BuildSteps.NewBuildStep step_name (Builds.NewBuildStepId step_id) build_obj maybe_breakage_obj
        build_obj = Builds.NewBuild (Builds.NewBuildNumber build_num) vcs_revision queued_at job_name branch
        maybe_breakage_obj = do
          is_broken <- maybe_is_broken
          notes <- maybe_notes
          reporter <- maybe_reporter
          return $ Breakages.NewBreakageReport (Builds.NewBuildStepId step_id) maybe_implicated_revision is_broken notes $ AuthStages.Username reporter

    sql = "SELECT step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, implicated_revision, is_broken, breakage_notes, reporter FROM builds_with_reports where build_num = ?;"


get_pattern_matches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_pattern_matches conn_data pattern_id = do
  rows <- get_pattern_occurrence_rows conn_data pattern_id
  return $ map f rows

  where
    f ((Builds.NewBuild buildnum vcs_rev queued_at job_name branch), stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) =
      PatternOccurrence buildnum pattern_id vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_pattern_occurrence_rows :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [(Builds.Build, Text, Int, ScanPatterns.MatchDetails)]
get_pattern_occurrence_rows conn_data (ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ map f xs

  where
    f (buildnum, stepname, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) vcs_revision queued_at job_name branch, stepname, line_count, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

    sql = "SELECT builds.build_num, step_name, line_number, line_count, line_text, span_start, span_end, builds.vcs_revision, queued_at, job_name, branch FROM matches_with_log_metadata JOIN builds ON matches_with_log_metadata.build_num = builds.build_num WHERE pattern = ?;"
