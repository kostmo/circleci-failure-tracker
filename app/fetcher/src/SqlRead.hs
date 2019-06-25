{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       except, runExceptT)
import           Data.Aeson
import           Data.Bifunctor                       (first)
import           Data.Coerce                          (coerce)
import           Data.Either.Utils                    (maybeToEither)
import           Data.List                            (sort, sortOn)
import           Data.List.Split                      (splitOn)
import qualified Data.Maybe                           as Maybe
import           Data.Scientific                      (Scientific)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time                            (UTCTime)
import           Data.Time.Calendar                   (Day)
import           Data.Tuple                           (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           GHC.Generics
import           GHC.Int                              (Int64)
import qualified Network.OAuth.OAuth2                 as OAuth2
import qualified Safe

import qualified AuthStages
import qualified BreakageReportsBackup
import qualified Breakages
import qualified BuildResults
import qualified Builds
import qualified BuildSteps
import qualified CommitBuilds
import qualified DbHelpers
import qualified GithubApiFetch
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Pagination
import qualified PostedStatuses
import qualified ScanPatterns
import qualified ScanUtils
import qualified StoredBreakageReports
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

    tags_list <- map (\(Only tag_text) -> tag_text) <$> query conn tags_sql (Only pattern_id)
    steps_list <- map (\(Only step_text) -> step_text) <$> query conn applicable_steps_sql (Only pattern_id)

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


api_posted_statuses :: DbHelpers.DbConnectionData -> IO [PostedStatuses.PostedStatus]
api_posted_statuses conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (sha1, description, state, created_at) = PostedStatuses.PostedStatus sha1 description state created_at
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
    _sha1       :: Builds.RawCommit
  , _test_name  :: Text
  , _build_date :: UTCTime
  } deriving Generic

instance ToJSON TestFailure where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | This uses capture groups of a specifically-crafted regex
-- to identify the name of the failing test
api_test_failures :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO (Either Text [TestFailure])
api_test_failures conn_data test_failure_pattern_id = do
  patterns_singleton <- api_single_pattern conn_data test_failure_pattern_id

  case Safe.headMay patterns_singleton of
    Nothing -> return $ Left "Could not find Test Failure pattern"
    Just test_failure_pattern -> do
      pattern_occurrences <- get_best_pattern_matches_whitelisted_branches conn_data test_failure_pattern_id
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
        maybe_first_match_group = ScanUtils.getFirstMatchGroup extracted_chunk pattern_text


-- XXX NOT USED
query_builds :: DbHelpers.DbConnectionData -> IO [Builds.Build]
query_builds conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (buildnum, vcs_rev, queuedat, jobname, branch) =
      Builds.NewBuild (Builds.NewBuildNumber buildnum) (Builds.RawCommit vcs_rev) queuedat jobname branch

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
  let inners = map (uncurry WebApi.PieSliceApiRecord) xs
  return $ WebApi.ApiResponse inners

  where
    sql = "SELECT step_name, COUNT(*) AS freq FROM builds_join_steps WHERE step_name IS NOT NULL AND branch IN (SELECT branch FROM presumed_stable_branches) GROUP BY step_name ORDER BY freq DESC;"


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


-- | TODO Don't hardcode is_broken to null; join tables instead
api_timeout_commit_builds :: DbHelpers.DbConnectionData -> Text -> IO [WebApi.UnmatchedBuild]
api_timeout_commit_builds conn_data sha1 = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, is_broken) = WebApi.UnmatchedBuild (Builds.NewBuildNumber build) step_name queued_at job_name branch is_broken
    sql = "SELECT build_num, step_name, queued_at, job_name, branch, NULL FROM builds_join_steps WHERE vcs_revision = ? AND is_timeout;"


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


get_latest_known_master_commit :: Connection -> IO (Maybe Text)
get_latest_known_master_commit conn = do
  rows <- query_ conn sql
  return $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = "SELECT sha1 FROM ordered_master_commits ORDER BY id DESC LIMIT 1;"


find_master_ancestor ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
find_master_ancestor conn_data access_token owner_and_repo sha1 = do

  conn <- DbHelpers.get_connection conn_data
  rows <- query_ conn sql
  let known_commit_set = Set.fromList $ map (\(Only x) -> x) rows

  merge_base_commit <- GithubApiFetch.findAncestor
    access_token
    owner_and_repo
    sha1
    known_commit_set

  return $ first TL.toStrict merge_base_commit

  where
    sql = "SELECT sha1 FROM ordered_master_commits;"


data CodeBreakage = CodeBreakage {
    _breakage_commit      :: Builds.RawCommit
  , _breakage_description :: Text
  , _jobs                 :: Set Text
  } deriving Generic

instance ToJSON CodeBreakage where
  toJSON = genericToJSON JsonUtils.dropUnderscore


get_master_commit_index ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text Int64)
get_master_commit_index conn (Builds.RawCommit sha1) = do
  rows <- query conn sql (Only sha1)
  return $ maybeToEither "Commit not found" $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = "SELECT id FROM ordered_master_commits WHERE sha1 = ?;"


get_spanning_breakages ::
     DbHelpers.DbConnectionData
  -> Builds.RawCommit
  -> IO (Either Text [DbHelpers.WithId CodeBreakage])
get_spanning_breakages conn_data sha1 = do

  conn <- DbHelpers.get_connection conn_data

  runExceptT $ do
    target_commit_index <- ExceptT $ get_master_commit_index conn sha1

    rows <- liftIO $ query conn sql (target_commit_index, target_commit_index)
    return $ map f rows

  where
    f (sha1, description, cause_id, jobs) = DbHelpers.WithId cause_id $
      CodeBreakage (Builds.RawCommit sha1) description $ Set.fromList $ map T.pack $ split_agg_text jobs

    sql = "SELECT code_breakage_cause.sha1, code_breakage_cause.description, cause_id, COALESCE(jobs, ''::text) AS jobs FROM (SELECT code_breakage_spans.cause_id, string_agg((code_breakage_affected_jobs.job)::text, ';'::text) AS jobs FROM code_breakage_spans LEFT JOIN code_breakage_affected_jobs ON code_breakage_affected_jobs.cause = code_breakage_spans.cause_id WHERE cause_commit_index <= ? AND (resolved_commit_index IS NULL OR ? < resolved_commit_index) GROUP BY code_breakage_spans.cause_id) foo JOIN code_breakage_cause ON foo.cause_id = code_breakage_cause.id"


list_flat :: (ToField b, FromField a) =>
     Query
  -> DbHelpers.DbConnectionData
  -> b
  -> IO [a]
list_flat sql conn_data t = do
  conn <- DbHelpers.get_connection conn_data
  map (\(Only x) -> x) <$> query conn sql (Only t)


api_autocomplete_tags :: DbHelpers.DbConnectionData -> Text -> IO [Text]
api_autocomplete_tags = list_flat sql
  where
    sql = "SELECT tag FROM (SELECT tag, COUNT(*) AS freq FROM pattern_tags GROUP BY tag ORDER BY freq DESC, tag ASC) foo WHERE tag ILIKE CONCAT(?,'%');"


api_autocomplete_steps :: DbHelpers.DbConnectionData -> Text -> IO [Text]
api_autocomplete_steps = list_flat sql
  where
    sql = "SELECT name FROM (SELECT name, COUNT(*) AS freq FROM build_steps where name IS NOT NULL GROUP BY name ORDER BY freq DESC, name ASC) foo WHERE name ILIKE CONCAT(?,'%');"


api_list_steps :: DbHelpers.DbConnectionData -> IO [Text]
api_list_steps conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map (\(Only x) -> x) <$> query_ conn sql
  where
    sql = "SELECT name FROM build_steps WHERE name IS NOT NULL GROUP BY name ORDER BY COUNT(*) DESC, name ASC;"


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
  fmap (map f) $ query conn sql $ Only $ GitRev.sha1 git_revision

  where
    f (step_name, match_id, buildnum, vcs_rev, queuedat, jobname, branch, patt, line_number, line_count, line_text, span_start, span_end, specificity, maybe_is_broken, maybe_reporter, maybe_report_timestamp) =
      CommitBuilds.NewCommitBuild
        build_obj
        match_obj
        (maybe_breakage_report maybe_is_broken  maybe_reporter maybe_report_timestamp)
      where
        build_obj = Builds.NewBuild (Builds.NewBuildNumber buildnum) (Builds.RawCommit vcs_rev) queuedat jobname branch
        match_obj = MatchOccurrences.MatchOccurrencesForBuild step_name (ScanPatterns.PatternId patt) (MatchOccurrences.MatchId match_id) line_number line_count line_text span_start span_end specificity

        maybe_breakage_report :: Maybe Bool -> Maybe Text -> Maybe UTCTime -> Maybe CommitBuilds.StoredBreakageReport
        maybe_breakage_report x y z = CommitBuilds.StoredBreakageReport
          <$> x
          <*> (AuthStages.Username <$> y)
          <*> z

    sql = "SELECT step_name, match_id, build, vcs_revision, queued_at, job_name, branch, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, is_broken, reporter, report_timestamp FROM best_pattern_match_augmented_builds WHERE vcs_revision = ?;"


get_master_commits ::
     Connection
  -> Pagination.OffsetLimit
  -> IO [BuildResults.IndexedCommit]
get_master_commits conn (Pagination.OffsetLimit offset_count commit_count) =
  map f <$> query conn sql (offset_count, commit_count)
  where
    f (my_id, my_commit) = DbHelpers.WithId my_id $ Builds.RawCommit my_commit
    sql = "SELECT id, sha1 FROM ordered_master_commits ORDER BY id DESC OFFSET ? LIMIT ?"


-- | Gets last N commits in one query,
-- then gets the list of jobs that apply to those commits,
-- then gets the associated builds
api_master_builds ::
     DbHelpers.DbConnectionData
  -> Pagination.OffsetLimit
  -> IO BuildResults.MasterBuildsResponse
api_master_builds conn_data offset_limit = do

  conn <- DbHelpers.get_connection conn_data

  master_commits <- get_master_commits conn offset_limit
  let last_row_id = maybe 0 DbHelpers.db_id $ Safe.lastMay master_commits

  job_names <- list_flat job_names_sql conn_data last_row_id
  failure_rows <- query conn failures_sql $ Only last_row_id

  code_breakage_ranges <- get_code_breakage_ranges conn

  return $ BuildResults.MasterBuildsResponse job_names master_commits
    (map f failure_rows) code_breakage_ranges

  where

    f (sha1, build_id, queued_at, job_name, branch, step_name, match_id, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, is_flaky) = BuildResults.SimpleBuildStatus
      build_obj
      (BuildResults.PatternMatch match_obj)
      is_flaky
      where
        build_obj = Builds.NewBuild
          (Builds.NewBuildNumber build_id)
          (Builds.RawCommit sha1)
          queued_at
          job_name
          branch

        match_obj = MatchOccurrences.MatchOccurrencesForBuild step_name (ScanPatterns.PatternId pattern_id) (MatchOccurrences.MatchId match_id) line_number line_count line_text span_start span_end specificity

    job_names_sql = "SELECT DISTINCT job_name FROM ordered_master_commits JOIN best_pattern_match_augmented_builds ON ordered_master_commits.sha1 = best_pattern_match_augmented_builds.vcs_revision WHERE ordered_master_commits.id >= ?;"

    failures_sql = "SELECT ordered_master_commits.sha1, build, queued_at, job_name, branch, step_name, match_id, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, is_flaky FROM ordered_master_commits JOIN best_pattern_match_augmented_builds ON ordered_master_commits.sha1 = best_pattern_match_augmented_builds.vcs_revision WHERE ordered_master_commits.id >= ?;"


-- | TODO What to do when multiple resolutions are assigned to the same cause?
get_code_breakage_span_end ::
     Connection
  -> Int64
  -> IO (Maybe BuildResults.BreakageEndRecord)
get_code_breakage_span_end conn cause_id = do

  rows <- query conn resolutions_sql $ Only cause_id

  breakage_ends <- forM rows $ \(commit_index, resolution_id, sha1, reporter, reported_at) -> do

    affected_jobs <- map f <$> query conn affected_cause_jobs_sql (Only resolution_id)

    return $ DbHelpers.WithId resolution_id $ DbHelpers.WithAuthorship reporter reported_at $
      BuildResults.BreakageEnd
        (DbHelpers.WithId commit_index $ Builds.RawCommit sha1)
        cause_id
        affected_jobs

  return $ Safe.headMay breakage_ends

  where
    f (job, reporter, reported_at) = DbHelpers.WithAuthorship reporter reported_at job

    resolutions_sql = "SELECT ordered_master_commits.id AS commit_index, code_breakage_resolution.id AS resolution_id, code_breakage_resolution.sha1, reporter, reported_at FROM code_breakage_resolution JOIN ordered_master_commits ON code_breakage_resolution.sha1 = ordered_master_commits.sha1 WHERE cause = ? ORDER BY resolution_id DESC LIMIT 1;"

    affected_cause_jobs_sql = "SELECT job, reporter, reported_at FROM code_breakage_resolved_jobs WHERE resolution = ?;"


-- | TODO replace with a query from the view "code_breakage_spans"
get_code_breakage_ranges ::
     Connection
  -> IO [BuildResults.BreakageSpan]
get_code_breakage_ranges conn = do

  rows <- query_ conn causes_sql

  forM rows $ \(commit_index, cause_id, sha1, description, reporter, reported_at) -> do

    affected_jobs <- map f <$> query conn affected_cause_jobs_sql (Only cause_id)

    let span_start = DbHelpers.WithId cause_id $ DbHelpers.WithAuthorship reporter reported_at $
          BuildResults.BreakageStart
            (DbHelpers.WithId commit_index $ Builds.RawCommit sha1)
            description
            affected_jobs

    maybe_span_end <- get_code_breakage_span_end conn cause_id

    return $ BuildResults.BreakageSpan span_start maybe_span_end

  where
    f (job, reporter, reported_at) = DbHelpers.WithAuthorship reporter reported_at job

    causes_sql = "SELECT ordered_master_commits.id AS commit_index, code_breakage_cause.id AS cause_id, code_breakage_cause.sha1, description, reporter, reported_at FROM code_breakage_cause JOIN ordered_master_commits ON code_breakage_cause.sha1 = ordered_master_commits.sha1;"

    affected_cause_jobs_sql = "SELECT job, reporter, reported_at FROM code_breakage_affected_jobs WHERE cause = ?;"


data CommitInfoCounts = NewCommitInfoCounts {
    _failed_build_count  :: Int
  , _timeout_count       :: Int
  , _matched_build_count :: Int
  , _code_breakage_count :: Int
  , _flaky_build_count   :: Int
  } deriving Generic

instance ToJSON CommitInfoCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data CommitInfo = NewCommitInfo {
    _counts  :: CommitInfoCounts
  } deriving Generic

instance ToJSON CommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


count_revision_builds :: DbHelpers.DbConnectionData -> GitRev.GitSha1 -> IO CommitInfo
count_revision_builds conn_data git_revision = do
  conn <- DbHelpers.get_connection conn_data
  [Only failed_count] <- query conn failed_count_sql only_commit
  [Only matched_count] <- query conn matched_count_sql only_commit
  [Only timeout_count] <- query conn timeout_count_sql only_commit
  [Only reported_count] <- query conn reported_broken_count_sql only_commit

  revision_builds <- get_revision_builds conn_data git_revision
  flaky_pattern_ids <- get_flaky_pattern_ids conn

  let is_flaky = (`Set.member` flaky_pattern_ids) . coerce . MatchOccurrences._pattern_id . CommitBuilds._match
      flaky_builds = filter is_flaky revision_builds
      flaky_build_count = length flaky_builds

  return $ NewCommitInfo $ NewCommitInfoCounts
    failed_count
    timeout_count
    matched_count
    reported_count
    flaky_build_count

  where
    sha1 = GitRev.sha1 git_revision
    only_commit = Only sha1

    timeout_count_sql = "SELECT COUNT(*) FROM builds_join_steps WHERE vcs_revision = ? AND is_timeout;"
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
    apply_pattern line_tuple = ScanUtils.applySinglePattern line_tuple $ DbHelpers.WithId 0 new_pattern


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


api_summary_stats :: DbHelpers.DbConnectionData -> IO SummaryStats
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
  fmap make_pattern_records $ query conn sql $ Only pattern_id
  where
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary WHERE id = ?;"


api_patterns :: DbHelpers.DbConnectionData -> IO [PatternRecord]
api_patterns conn_data = do
  conn <- DbHelpers.get_connection conn_data
  make_pattern_records <$> query_ conn sql
  where
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary ORDER BY most_recent DESC NULLS LAST;"


-- | For the purpose of database upgrades
dump_presumed_stable_branches :: DbHelpers.DbConnectionData -> IO [Text]
dump_presumed_stable_branches conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map (\(Only x) -> x) <$> query_ conn sql
  where
    sql = "SELECT branch FROM presumed_stable_branches ORDER BY branch;"


-- | For the purpose of database upgrades
dump_patterns :: DbHelpers.DbConnectionData -> IO [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
dump_patterns conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql

  where
    f (author, created, pattern_id, is_regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end) =
      DbHelpers.WithAuthorship author created $ wrap_pattern pattern_id is_regex expression has_nondeterministic_values description
        (sort $ map T.pack $ split_agg_text tags)
        (sort $ map T.pack $ split_agg_text steps)
        specificity
        is_retired
        lines_from_end

    sql = "SELECT author, created, id, regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end FROM patterns_augmented ORDER BY id;"


-- | For the purpose of database upgrades
dump_breakages :: DbHelpers.DbConnectionData -> IO [DbHelpers.WithId BreakageReportsBackup.DbBreakageReport]
dump_breakages conn_data = do
  conn <- DbHelpers.get_connection conn_data
  map f <$> query_ conn sql
  where
    f (id, reporter, reported_at, build_step, is_broken, implicated_revision, notes) = DbHelpers.WithId id $ BreakageReportsBackup.DbBreakageReport (AuthStages.Username reporter) reported_at (Builds.NewBuildStepId build_step) is_broken implicated_revision notes
    sql = "SELECT id, reporter, reported_at, build_step, is_broken, implicated_revision, notes FROM broken_build_reports ORDER BY id;"


-- | Note that this SQL is from decomposing the "pattern_frequency_summary" and "aggregated_build_matches" view
-- to parameterize the latter by branch.
api_patterns_branch_filtered :: DbHelpers.DbConnectionData -> [Text] -> IO [PatternRecord]
api_patterns_branch_filtered conn_data branches = do
  conn <- DbHelpers.get_connection conn_data
  fmap make_pattern_records $ query conn sql $ Only $ In branches

  where
    sql = "SELECT patterns_augmented.id, patterns_augmented.regex, patterns_augmented.expression, patterns_augmented.description, COALESCE(aggregated_build_matches.matching_build_count, 0::int) AS matching_build_count, aggregated_build_matches.most_recent, aggregated_build_matches.earliest, patterns_augmented.tags, patterns_augmented.steps, patterns_augmented.specificity, CAST((patterns_augmented.scanned_count * 100 / patterns_augmented.total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM patterns_augmented LEFT JOIN ( SELECT best_pattern_match_for_builds.pattern_id AS pat, count(best_pattern_match_for_builds.build) AS matching_build_count, max(builds.queued_at) AS most_recent, min(builds.queued_at) AS earliest FROM best_pattern_match_for_builds JOIN builds ON builds.build_num = best_pattern_match_for_builds.build WHERE builds.branch IN ? GROUP BY best_pattern_match_for_builds.pattern_id) aggregated_build_matches ON patterns_augmented.id = aggregated_build_matches.pat ORDER BY matching_build_count DESC;"


data PatternOccurrence = NewPatternOccurrence {
    _build_number :: Builds.BuildNumber
  , _pattern_id   :: ScanPatterns.PatternId
  , _match_id     :: MatchOccurrences.MatchId
  , _vcs_revision :: Builds.RawCommit
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
    f (step_name, patt, match_id, line_number, line_count, line_text, span_start, span_end, specificity) =
      MatchOccurrences.MatchOccurrencesForBuild
        step_name (ScanPatterns.PatternId patt) (MatchOccurrences.MatchId match_id) line_number line_count line_text span_start span_end specificity

    sql = "SELECT step_name, pattern, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, specificity FROM matches_with_log_metadata JOIN build_steps ON matches_with_log_metadata.build_step = build_steps.id JOIN patterns_augmented ON patterns_augmented.id = matches_with_log_metadata.pattern WHERE matches_with_log_metadata.build_num = ? ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"


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


pattern_occurrence_txform pattern_id = txform . f
  where
    -- TODO consolidate this transformation with "get_pattern_matches"
    f (buildnum, stepname, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) (Builds.RawCommit vcs_revision) queued_at job_name branch, stepname, line_count, MatchOccurrences.MatchId match_id, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

    txform (Builds.NewBuild buildnum vcs_rev queued_at job_name branch, stepname, line_count, match_id, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = NewPatternOccurrence buildnum pattern_id match_id vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_best_pattern_matches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_best_pattern_matches conn_data pat@(ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only pattern_id
  return $ map (pattern_occurrence_txform pat) xs

  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE pattern_id = ?;"


get_best_pattern_matches_whitelisted_branches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_best_pattern_matches_whitelisted_branches conn_data pat@(ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data

  xs <- query conn sql $ Only pattern_id
  return $ map (pattern_occurrence_txform pat) xs

  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE pattern_id = ? AND branch IN (SELECT branch from presumed_stable_branches);"


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
    f (pattern_id, build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) = pattern_occurrence_txform (ScanPatterns.PatternId pattern_id) (build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch)

    sql = "SELECT pattern_id, build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE build = ?;"


data LogContext = LogContext {
    _match_info   :: ScanPatterns.MatchDetails
  , _log_lines    :: [(Int, Text)]
  , _build_number :: Builds.BuildNumber
  } deriving Generic

instance ToJSON LogContext where
  toJSON = genericToJSON JsonUtils.dropUnderscore


log_context_func :: DbHelpers.DbConnectionData -> MatchOccurrences.MatchId -> Int -> IO (Either Text LogContext)
log_context_func connection_data (MatchOccurrences.MatchId match_id) context_linecount = do
  conn <- DbHelpers.get_connection connection_data

  xs <- query conn sql $ Only match_id
  let maybe_first_row = Safe.headMay xs

  runExceptT $ do
    first_row <- except $ maybeToEither (T.pack $ "Match ID " ++ show match_id ++ " not found") maybe_first_row

    let (build_num, line_number, span_start, span_end, line_text) = first_row
        match_info = ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end
        wrapped_build_num = Builds.NewBuildNumber build_num

    maybe_log <- liftIO $ SqlRead.read_log conn wrapped_build_num
    console_log <- except $ maybeToEither "log not in database" maybe_log

    let log_lines = T.lines console_log

        first_context_line = max 0 $ line_number - context_linecount

        tuples = zip [first_context_line..] $ take (2*context_linecount + 1) $ drop first_context_line log_lines

    return $ LogContext match_info tuples wrapped_build_num

  where
    sql = "SELECT build_num, line_number, span_start, span_end, line_text FROM matches_with_log_metadata WHERE id = ?"


data SingleBuildInfo = SingleBuildInfo {
    _multi_match_count :: Int
  , _build_info        :: BuildSteps.BuildStep
  , _known_failures    :: [DbHelpers.WithId CodeBreakage]
  } deriving Generic

instance ToJSON SingleBuildInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


get_build_info ::
     DbHelpers.DbConnectionData
  -> Builds.BuildNumber
  -> IO (Either Text SingleBuildInfo)
get_build_info conn_data build@(Builds.NewBuildNumber build_id) = do

  conn <- DbHelpers.get_connection conn_data
  xs <- query conn sql $ Only build_id

  -- TODO Replace this with SQL COUNT()
  matches <- get_build_pattern_matches conn_data build

  let either_tuple = f (length matches) <$> maybeToEither (T.pack $ "Build with ID " ++ show build_id ++ " not found!") (Safe.headMay xs)

  runExceptT $ do
    (multi_match_count, step_container) <- except either_tuple

    let sha1 = Builds.vcs_revision $ BuildSteps.build step_container
        job_name = Builds.job_name $ BuildSteps.build step_container
    breakages <- ExceptT $ get_spanning_breakages conn_data sha1
    let applicable_breakages = filter (Set.member job_name . _jobs . DbHelpers.record) breakages
    return $ SingleBuildInfo multi_match_count step_container applicable_breakages

  where
    f multi_match_count (step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, maybe_implicated_revision, maybe_is_broken, maybe_notes, maybe_reporter) = (multi_match_count, step_container)
      where
        step_container = BuildSteps.NewBuildStep step_name (Builds.NewBuildStepId step_id) build_obj maybe_breakage_obj
        build_obj = Builds.NewBuild (Builds.NewBuildNumber build_num) (Builds.RawCommit vcs_revision) queued_at job_name branch
        maybe_breakage_obj = do
          is_broken <- maybe_is_broken
          notes <- maybe_notes
          reporter <- maybe_reporter
          return $ Breakages.NewBreakageReport (Builds.NewBuildStepId step_id) maybe_implicated_revision is_broken notes $ AuthStages.Username reporter

    sql = "SELECT step_id, step_name, build_num, vcs_revision, queued_at, job_name, branch, implicated_revision, is_broken, breakage_notes, reporter FROM builds_with_reports where build_num = ?;"


get_pattern_matches :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [PatternOccurrence]
get_pattern_matches conn_data pattern_id =
  map f <$> get_pattern_occurrence_rows conn_data pattern_id

  where
    f (Builds.NewBuild buildnum vcs_rev queued_at job_name branch, stepname, line_count, match_id, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) =
      NewPatternOccurrence buildnum pattern_id match_id vcs_rev queued_at job_name branch stepname line_number line_count line_text start end


get_pattern_occurrence_rows :: DbHelpers.DbConnectionData -> ScanPatterns.PatternId -> IO [(Builds.Build, Text, Int, MatchOccurrences.MatchId, ScanPatterns.MatchDetails)]
get_pattern_occurrence_rows conn_data (ScanPatterns.PatternId pattern_id) = do

  conn <- DbHelpers.get_connection conn_data
  fmap (map f) $ query conn sql $ Only pattern_id

  where
    f (buildnum, stepname, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) (Builds.RawCommit vcs_revision) queued_at job_name branch, stepname, line_count, MatchOccurrences.MatchId match_id, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end)

    sql = "SELECT builds.build_num, step_name, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, builds.vcs_revision, queued_at, job_name, branch FROM matches_with_log_metadata JOIN builds ON matches_with_log_metadata.build_num = builds.build_num WHERE pattern = ?;"
