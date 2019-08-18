{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Monad                        (forM)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       except, runExceptT)
import           Control.Monad.Trans.Reader           (ReaderT, ask)
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
import qualified Data.Text.Lazy                       as LT
import           Data.Time                            (UTCTime)
import           Data.Time.Calendar                   (Day)
import           Data.Tuple                           (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow   (field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           GHC.Generics
import           GHC.Int                              (Int64)
import qualified Safe

import qualified AuthStages
import qualified BuildResults
import qualified Builds
import qualified CommitBuilds
import qualified Commits
import qualified DbHelpers
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified MyUtils
import qualified Pagination
import qualified PostedStatuses
import qualified ScanPatterns
import qualified ScanUtils
import qualified WebApi
import qualified WeeklyStats


circleCIProviderIndex :: Int64
circleCIProviderIndex = 3


type DbIO a = ReaderT Connection IO a


type AuthDbIO a = ReaderT AuthConnection IO a


-- | For use with ReaderT
data AuthConnection = AuthConnection {
    getConn :: Connection
  , getUser :: AuthStages.Username
  }


runQuery sql = do
  conn <- ask
  liftIO $ query_ conn sql


constructExpression :: Bool -> Text -> Bool -> ScanPatterns.MatchExpression
constructExpression
    is_regex
    pattern_text
    is_nondeterministic = if is_regex
  then ScanPatterns.RegularExpression pattern_text is_nondeterministic
  else ScanPatterns.LiteralExpression pattern_text


wrapPattern ::
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
wrapPattern pattern_id is_regex pattern_text is_nondeterministic description tags_list steps_list specificity is_retired maybe_lines_from_end =
  DbHelpers.WithId pattern_id inner_pattern
  where
    expression_obj = constructExpression is_regex pattern_text is_nondeterministic
    inner_pattern = ScanPatterns.NewPattern expression_obj description tags_list steps_list specificity is_retired maybe_lines_from_end


getPatterns :: Connection -> IO [ScanPatterns.DbPattern]
getPatterns conn = do

  patterns_rows <- query_ conn patterns_sql

  forM patterns_rows $ \(pattern_id, is_regex, pattern_text, has_nondeterministic_values, description, specificity, is_retired, lines_from_end) -> do

    tags_list <- map (\(Only tag_text) -> tag_text) <$> query conn tags_sql (Only pattern_id)
    steps_list <- map (\(Only step_text) -> step_text) <$> query conn applicable_steps_sql (Only pattern_id)

    return $ wrapPattern pattern_id is_regex pattern_text has_nondeterministic_values description tags_list steps_list specificity is_retired lines_from_end

  where
    patterns_sql = "SELECT id, regex, expression, has_nondeterministic_values, description, specificity, is_retired, lines_from_end FROM patterns ORDER BY description;"

    tags_sql = "SELECT tag FROM pattern_tags WHERE pattern = ?;"
    applicable_steps_sql = "SELECT step_name FROM pattern_step_applicability WHERE pattern = ?;"


-- | Only searches for CircleCI builds
getUnvisitedBuildIds ::
     Connection
  -> Maybe Int
  -> IO [DbHelpers.WithId Builds.UniversalBuild]
getUnvisitedBuildIds conn maybe_limit = do
  rows <- case maybe_limit of
    Just limit -> query conn sql (circleCIProviderIndex, limit)
    Nothing    -> query conn unlimited_sql $ Only circleCIProviderIndex
  return $ map f rows
  where

    f (universal_build_id, provider_buildnum, provider_id, build_namespace, succeeded, sha1) = DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber provider_buildnum)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit sha1)

    sql = "SELECT universal_build_id, build_num, provider, build_namespace, succeeded, commit_sha1 FROM unvisited_builds WHERE provider = ? ORDER BY build_num DESC LIMIT ?;"

    unlimited_sql = "SELECT universal_build_id, build_num, provider, build_namespace, succeeded, commit_sha1 FROM unvisited_builds WHERE provider = ? ORDER BY build_num DESC;"


-- | TODO Use this
lookupUniversalBuildFromProviderBuild ::
     Connection
  -> Builds.BuildNumber
  -> IO (Maybe (DbHelpers.WithId Builds.UniversalBuild))
lookupUniversalBuildFromProviderBuild conn (Builds.NewBuildNumber build_num) = do
  rows <- query conn sql $ Only build_num
  return $ Safe.headMay rows
  where
    sql = "SELECT global_build_num, build_number, provider, build_namespace, succeeded, vcs_revision, queued_at, job_name, branch, started_at, finished_at FROM global_builds WHERE build_number = ? ORDER BY provider DESC, global_build_num DESC LIMIT 1;"


-- | FIXME partial function
lookupUniversalBuild ::
     Builds.UniversalBuildId -- ^ oldest build number
  -> DbIO (DbHelpers.WithId Builds.UniversalBuild)
lookupUniversalBuild (Builds.UniversalBuildId universal_build_num) = do
  conn <- ask
  liftIO $ head <$> query conn sql (Only universal_build_num)
  where
    sql = "SELECT id, build_number, provider, build_namespace, succeeded, commit_sha1 FROM universal_builds WHERE id = ?;"


getUniversalBuilds ::
     Builds.UniversalBuildId -- ^ oldest build number
  -> Int -- ^ limit
  -> DbIO [DbHelpers.WithId Builds.UniversalBuild]
getUniversalBuilds (Builds.UniversalBuildId oldest_universal_build_num) limit = do
  conn <- ask
  liftIO $ query conn sql (oldest_universal_build_num, limit)
  where
    sql = "SELECT id, build_number, provider, build_namespace, succeeded, commit_sha1 FROM universal_builds WHERE id >= ? ORDER BY id ASC LIMIT ?;"


-- | XXX This is a partial function
getGlobalBuild ::
     Connection
  -> Builds.UniversalBuildId
  -> IO Builds.StorableBuild
getGlobalBuild conn (Builds.UniversalBuildId global_build_num) = do
  [x] <- query conn sql $ Only global_build_num
  return x
  where
    sql = "SELECT global_build_num, build_number, provider, build_namespace, succeeded, vcs_revision, queued_at, job_name, branch, started_at, finished_at FROM global_builds WHERE global_build_num = ?;"


common_xform (delimited_pattern_ids, step_id, step_name, universal_build_id, build_num, provider_id, build_namespace, succeeded, vcs_revision) =
  ( Builds.NewBuildStepId step_id
  , step_name
  , DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber build_num)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit vcs_revision)
  , map read $ splitOn ";" delimited_pattern_ids
  )


getRevisitableWhitelistedBuilds ::
     Connection
  -> [Builds.UniversalBuildId]
  -> IO [(Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
getRevisitableWhitelistedBuilds conn universal_build_ids = do
  putStrLn $ unwords ["Inside", "getRevisitableWhitelistedBuilds"]
  map common_xform <$> query conn sql (Only $ In $ map (\(Builds.UniversalBuildId x) -> x) universal_build_ids)
  where
    sql = "SELECT unscanned_patterns_delimited, step_id, step_name, universal_build, build_num, provider, build_namespace, succeeded, vcs_revision FROM unscanned_patterns WHERE universal_build IN ?;"


getRevisitableBuilds ::
     Connection
  -> IO [(Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
getRevisitableBuilds conn =
  map common_xform <$> query_ conn sql
  where
    sql = "SELECT unscanned_patterns_delimited, step_id, step_name, universal_build, build_num, provider, build_namespace, succeeded, vcs_revision FROM unscanned_patterns;"


-- | FIXME don't use partial "head"
getLatestPatternId :: Connection -> IO ScanPatterns.PatternId
getLatestPatternId conn =
  head <$> query_ conn sql
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1;"


apiPostedStatuses :: Int -> DbIO [PostedStatuses.PostedStatus]
apiPostedStatuses count = do
  conn <- ask
  liftIO $ query conn sql $ Only count
  where
    sql = "SELECT sha1, description, state, created_at FROM created_github_statuses ORDER BY created_at DESC LIMIT ?;"


apiAggregatePostedStatuses :: Int -> DbIO [PostedStatuses.PostedStatusAggregate]
apiAggregatePostedStatuses count = do
  conn <- ask
  liftIO $ query conn sql $ Only count
  where
    sql = "SELECT sha1, count, last_time, EXTRACT(SECONDS FROM time_interval) FROM aggregated_github_status_postings LIMIT ?;"


data PatternsTimelinePoint = PatternsTimelinePoint {
    _pattern_id :: Int64
  , _count      :: Int
  , _week       :: UTCTime
  } deriving (Generic, FromRow)

instance ToJSON PatternsTimelinePoint where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data PatternsTimeline = PatternsTimeline {
    _patterns :: [PatternRecord]
  , _points   :: [PatternsTimelinePoint]
  } deriving Generic

instance ToJSON PatternsTimeline where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiPatternOccurrenceTimeline :: DbIO PatternsTimeline
apiPatternOccurrenceTimeline = do
  conn <- ask
  patterns <- apiPatterns
  liftIO $ do
    points <- query_ conn timeline_sql

    let filtered_patterns = sortOn (negate . _frequency) $ filter ((> 0) . _frequency) patterns
    return $ PatternsTimeline filtered_patterns points
  where
    timeline_sql = "SELECT pattern_id, COUNT(*) AS occurrences, date_trunc('week', queued_at) AS week FROM best_pattern_match_augmented_builds WHERE branch IN (SELECT branch FROM presumed_stable_branches) GROUP BY pattern_id, week"


data TestFailure = TestFailure {
    _sha1       :: Builds.RawCommit
  , _test_name  :: Text
  , _build_date :: UTCTime
  } deriving Generic

instance ToJSON TestFailure where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | This uses capture groups of a specifically-crafted regex
-- to identify the name of the failing test
apiTestFailures :: ScanPatterns.PatternId -> DbIO (Either Text [TestFailure])
apiTestFailures test_failure_pattern_id = do
  patterns_singleton <- apiSinglePattern test_failure_pattern_id

  case Safe.headMay patterns_singleton of
    Nothing -> return $ Left "Could not find Test Failure pattern"
    Just test_failure_pattern -> do
      pattern_occurrences <- getBestPatternMatchesWhitelistedBranches test_failure_pattern_id
      return $ Right $ Maybe.mapMaybe (repackage test_failure_pattern) pattern_occurrences

  where
    repackage test_failure_pattern pattern_occurrence = do
      maybe_first_match <- maybe_first_match_group
      return $ TestFailure
        (_vcs_revision pattern_occurrence)
        (T.pack maybe_first_match)
        (_queued_at pattern_occurrence)
      where
        start_idx = fromIntegral $ _span_start pattern_occurrence
        end_idx = fromIntegral $ _span_end pattern_occurrence
        span_length = end_idx - start_idx
        extracted_chunk = LT.take span_length $ LT.drop start_idx $ _line_text pattern_occurrence

        pattern_text = _pattern test_failure_pattern
        maybe_first_match_group = ScanUtils.getFirstMatchGroup extracted_chunk pattern_text


patternBuildStepOccurrences ::
     ScanPatterns.PatternId
  -> DbIO [WebApi.PieSliceApiRecord]
patternBuildStepOccurrences (ScanPatterns.PatternId patt) = do
  conn <- ask
  liftIO $ query conn sql $ Only patt
  where
    sql = "SELECT name, occurrence_count FROM pattern_build_step_occurrences WHERE pattern = ? ORDER BY occurrence_count DESC, name ASC;"


patternBuildJobOccurrences :: ScanPatterns.PatternId -> DbIO [WebApi.PieSliceApiRecord]
patternBuildJobOccurrences (ScanPatterns.PatternId patt) = do
  conn <- ask
  liftIO $ query conn sql $ Only patt
  where
    sql = "SELECT job_name, occurrence_count FROM pattern_build_job_occurrences WHERE pattern = ? ORDER BY occurrence_count DESC, job_name ASC;"


apiLineCountHistogram :: DbIO [(Text, Int)]
apiLineCountHistogram = map (swap . f) <$> runQuery
  "select count(*) as qty, pow(10, floor(ln(line_count) / ln(10)))::numeric::integer as bin from log_metadata WHERE line_count > 0 group by bin ORDER BY bin ASC;"
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)


apiByteCountHistogram :: DbIO [(Text, Int)]
apiByteCountHistogram = map (swap . f) <$> runQuery
  "select count(*) as qty, pow(10, floor(ln(byte_count) / ln(10)))::numeric::integer as bin from log_metadata WHERE byte_count > 0 group by bin ORDER BY bin ASC;"
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)


data JobBuild = JobBuild {
    _job                :: Text
  , _build              :: Builds.BuildNumber
  , _flaky              :: Bool
  , _known_broken       :: Bool
  , _universal_build_id :: Builds.UniversalBuildId
  , _provider_id        :: Int64
  , _occurrence_count   :: Int
  } deriving (Generic, FromRow)

instance ToJSON JobBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | Lists jobs affected by failure cause
apiCommitJobs ::
     Builds.RawCommit
  -> DbIO [JobBuild]
apiCommitJobs (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = "SELECT job_name, build_num, is_flaky, is_known_broken, global_build, provider, 1 FROM build_failure_causes WHERE vcs_revision = ? ORDER BY job_name;"


getNextMasterCommit ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
getNextMasterCommit conn (Builds.RawCommit current_git_revision) = do
  rows <- query conn sql $ Only current_git_revision

  let mapped_rows = map (\(Only x) -> Builds.RawCommit x) rows
  return $ maybeToEither ("There are no commits that come after " <> current_git_revision) $ Safe.headMay mapped_rows
  where
    sql = "SELECT sha1 FROM ordered_master_commits WHERE id > (SELECT id FROM ordered_master_commits WHERE sha1 = ?) ORDER BY id ASC LIMIT 1"


data InclusiveSpan = InclusiveSpan {
    first_value :: Int
  , last_value  :: Int
  } deriving Generic


apiCommitRangeJobs ::
     InclusiveSpan
  -> DbIO [JobBuild]
apiCommitRangeJobs (InclusiveSpan first_index last_index) = do
  conn <- ask
  liftIO $ query conn sql (first_index, last_index)
  where
    sql = "SELECT DISTINCT ON (job_name) job_name, build_num, is_flaky, is_known_broken, global_build, provider, count(*) OVER (PARTITION BY job_name) AS job_occurrences FROM (SELECT sha1 FROM ordered_master_commits WHERE id >= ? AND id <= ?) foo JOIN build_failure_causes ON build_failure_causes.vcs_revision = foo.sha1"


apiJobs :: DbIO (WebApi.ApiResponse WebApi.JobApiRecord)
apiJobs = WebApi.ApiResponse . map f <$> runQuery
  "SELECT job_name, freq FROM job_failure_frequencies;"
  where
    f (jobname, freq) = WebApi.JobApiRecord jobname [freq]


apiStep :: DbIO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
apiStep = WebApi.ApiResponse <$> runQuery
  "SELECT step_name, COUNT(*) AS freq FROM builds_join_steps WHERE step_name IS NOT NULL AND branch IN (SELECT branch FROM presumed_stable_branches) GROUP BY step_name ORDER BY freq DESC;"


apiDeterministicFailureModes :: DbIO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
apiDeterministicFailureModes = WebApi.ApiResponse <$> runQuery
  "SELECT master_failure_modes.label, freq FROM (SELECT failure_mode_id, COUNT(*) AS freq FROM known_breakage_summaries GROUP BY failure_mode_id ORDER BY freq DESC) foo JOIN master_failure_modes on foo.failure_mode_id = master_failure_modes.id;"


-- | Note that Highcharts expects the dates to be in ascending order
apiFailedCommitsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiFailedCommitsByDay = WebApi.ApiResponse <$> runQuery
  "SELECT queued_at::date AS date, COUNT(*) FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM global_builds GROUP BY vcs_revision) foo GROUP BY date ORDER BY date ASC;"


-- | Note that Highcharts expects the dates to be in ascending order
apiStatusPostedCommitsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiStatusPostedCommitsByDay = WebApi.ApiResponse <$> runQuery
  "SELECT last_time::date AS date, COUNT(*) FROM aggregated_github_status_postings GROUP BY date ORDER BY date ASC;"


-- | Note that Highcharts expects the dates to be in ascending order
apiStatusPostingsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiStatusPostingsByDay = WebApi.ApiResponse <$> runQuery
  "SELECT created_at::date AS date, COUNT(*) FROM created_github_statuses GROUP BY date ORDER BY date ASC;"


getFlakyPatternIds :: Connection -> IO (Set Int64)
getFlakyPatternIds conn = do
  xs <- query_ conn sql
  return $ Set.fromList $ map (\(Only x) -> x) xs
  where
    sql = "SELECT id FROM flaky_patterns_augmented;"


listBuilds :: Query -> DbIO [WebApi.BuildBranchRecord]
listBuilds sql = do
  conn <- ask
  liftIO $ query_ conn sql


apiUnmatchedBuilds :: DbIO [WebApi.BuildBranchRecord]
apiUnmatchedBuilds = listBuilds
  "SELECT branch, global_build FROM unattributed_failed_builds ORDER BY global_build DESC;"


apiIdiopathicBuilds :: DbIO [WebApi.BuildBranchRecord]
apiIdiopathicBuilds = listBuilds
  "SELECT branch, global_build_num FROM idiopathic_build_failures ORDER BY global_build_num DESC;"


apiUnmatchedCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiUnmatchedCommitBuilds sha1 = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = "SELECT build_num, step_name, queued_at, job_name, unattributed_failed_builds.branch, builds_join_steps.universal_build, ci_providers.icon_url, ci_providers.label FROM unattributed_failed_builds JOIN builds_join_steps ON unattributed_failed_builds.global_build = builds_join_steps.universal_build JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ?"


apiIdiopathicCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiIdiopathicCommitBuilds sha1 = do
  conn <- ask
  liftIO $ map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, universal_build_id, provider_icon_url, provider_label) = WebApi.UnmatchedBuild (Builds.NewBuildNumber build) step_name queued_at job_name branch (Builds.UniversalBuildId universal_build_id) provider_icon_url provider_label
    sql = "SELECT build_num, step_name, queued_at, job_name, idiopathic_build_failures.branch, builds_join_steps.universal_build, ci_providers.icon_url, ci_providers.label FROM idiopathic_build_failures JOIN builds_join_steps ON idiopathic_build_failures.global_build_num = builds_join_steps.universal_build JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ?"


apiTimeoutCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiTimeoutCommitBuilds sha1 = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = "SELECT build_num, step_name, queued_at, job_name, branch, universal_build, ci_providers.icon_url, ci_providers.label FROM builds_join_steps JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ? AND is_timeout;"


-- | Obtains the console log from database
readLog :: Connection -> Builds.UniversalBuildId -> IO (Maybe LT.Text)
readLog conn (Builds.UniversalBuildId build_num) = do
  result <- query conn sql $ Only build_num
  return $ (\(Only log_text) -> log_text) <$> Safe.headMay result
  where
    sql = "SELECT log_metadata.content FROM log_metadata JOIN builds_join_steps ON log_metadata.step = builds_join_steps.step_id WHERE builds_join_steps.universal_build = ? LIMIT 1;"


data MasterBuildStats = MasterBuildStats {
    _total           :: Int
  , _idiopathic      :: Int
  , _timeout         :: Int
  , _known_broken    :: Int
  , _pattern_matched :: Int
  , _flaky           :: Int
  } deriving (Generic, FromRow)

instance ToJSON MasterBuildStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | TODO head is partial
masterBuildFailureStats :: DbIO MasterBuildStats
masterBuildFailureStats = head <$> runQuery
  "SELECT count(*) AS total, sum(is_idiopathic::int) AS idiopathic, sum(is_timeout::int) AS timeout, sum(is_known_broken::int) AS known_broken, sum((NOT is_unmatched)::int) AS pattern_matched, sum(is_flaky::int) AS flaky FROM build_failure_causes JOIN ordered_master_commits ON build_failure_causes.vcs_revision = ordered_master_commits.sha1"


-- | Uses OFFSET 1 so we only ever show full weeks
masterWeeklyFailureStats :: Int -> DbIO WeeklyStats.MasterStatsBundle
masterWeeklyFailureStats week_count = do

  conn <- ask
  xs <- liftIO $ query conn sql $ Only week_count
  return $ WeeklyStats.MasterStatsBundle
    WeeklyStats.buildCountColors
    (reverse $ map f xs)
  where
    sql = "SELECT commit_count, had_failure, had_idiopathic, had_timeout, had_known_broken, had_pattern_matched, had_flaky, failure_count::int, idiopathic_count::int, timeout_count::int, known_broken_count::int, pattern_matched_count::int, pattern_unmatched_count::int, flaky_count::int, earliest_commit_index, latest_commit_index, week FROM master_failures_weekly_aggregation ORDER BY week DESC LIMIT ? OFFSET 1"

    f (commit_count, had_failure, had_idiopathic, had_timeout, had_known_broken, had_pattern_matched, had_flaky, failure_count, idiopathic_count, timeout_count, known_broken_count, pattern_matched_count, pattern_unmatched_count, flaky_count, earliest_commit_index, latest_commit_index, week) =
      WeeklyStats.MasterWeeklyStats
        commit_count
        agg_commit_counts
        agg_build_counts
        week $ WeeklyStats.InclusiveNumericBounds earliest_commit_index latest_commit_index
      where
        agg_commit_counts :: WeeklyStats.AggregateCommitCounts Int
        agg_commit_counts = WeeklyStats.AggregateCommitCounts
          had_failure
          had_idiopathic
          had_timeout
          had_known_broken
          had_pattern_matched
          had_flaky

        agg_build_counts :: WeeklyStats.AggregateBuildCounts Int
        agg_build_counts = WeeklyStats.AggregateBuildCounts
          failure_count
          idiopathic_count
          timeout_count
          known_broken_count
          pattern_matched_count
          pattern_unmatched_count
          flaky_count


-- | Uses OFFSET 1 so we only ever show full weeks
downstreamWeeklyFailureStats :: Int -> DbIO [BuildResults.WeeklyBreakageImpactStats]
downstreamWeeklyFailureStats week_count = do

  conn <- ask
  xs <- liftIO $ query conn sql $ Only week_count
  return $ reverse $ map f xs
  where
    f (week, distinct_breakages, downstream_broken_commit_count, downstream_broken_build_count) = BuildResults.WeeklyBreakageImpactStats
      week
      distinct_breakages $ BuildResults.DownstreamImpactCounts
        downstream_broken_commit_count
        downstream_broken_build_count

    sql = "SELECT week, distinct_breakages, downstream_broken_commit_count, downstream_broken_build_count FROM upstream_breakages_weekly_aggregation ORDER BY week DESC LIMIT ? OFFSET 1;"


getLatestKnownMasterCommit :: Connection -> IO (Maybe Text)
getLatestKnownMasterCommit conn = do
  rows <- query_ conn sql
  return $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = "SELECT sha1 FROM ordered_master_commits ORDER BY id DESC LIMIT 1;"


getAllMasterCommits :: Connection -> IO (Set Builds.RawCommit)
getAllMasterCommits conn = do
  master_commit_rows <- query_ conn master_commit_retrieval_sql
  return $ Set.fromList $ map (\(Only x) -> x) master_commit_rows
  where
    master_commit_retrieval_sql = "SELECT sha1 FROM ordered_master_commits;"


data CodeBreakage = CodeBreakage {
    _breakage_commit      :: Builds.RawCommit
  , _breakage_description :: Text
  , _jobs                 :: Set Text
  } deriving Generic

instance ToJSON CodeBreakage where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getMasterCommitIndex ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text Int64)
getMasterCommitIndex conn (Builds.RawCommit sha1) = do
  rows <- query conn sql $ Only sha1
  return $ maybeToEither ("Commit " <> sha1 <>" not found in master branch") $
    Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = "SELECT id FROM ordered_master_commits WHERE sha1 = ?;"


knownBreakageAffectedJobs ::
     Int
  -> DbIO [DbHelpers.WithAuthorship Text]
knownBreakageAffectedJobs cause_id = do
  conn <- ask
  liftIO $ map f <$> query conn sql (Only cause_id)
  where
    f (reporter, reported_at, job) = DbHelpers.WithAuthorship reporter reported_at job
    sql = "SELECT reporter, reported_at, job FROM code_breakage_affected_jobs WHERE cause = ? ORDER BY job ASC"


-- | This only works for commits from the master branch.
-- Commits from other branches must use
-- StatusUpdate.findKnownBuildBreakages
getSpanningBreakages ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text [DbHelpers.WithId CodeBreakage])
getSpanningBreakages conn sha1 =

  runExceptT $ do
    target_commit_index <- ExceptT $ getMasterCommitIndex conn sha1

    rows <- liftIO $ query conn sql (target_commit_index, target_commit_index)
    return $ map f rows

  where
    f (sha1, description, cause_id, jobs) = DbHelpers.WithId cause_id $
      CodeBreakage (Builds.RawCommit sha1) description $ Set.fromList $
        map T.pack $ DbHelpers.splitAggText jobs

    sql = "SELECT code_breakage_cause.sha1, code_breakage_cause.description, cause_id, COALESCE(jobs, ''::text) AS jobs FROM (SELECT code_breakage_spans.cause_id, string_agg((code_breakage_affected_jobs.job)::text, ';'::text) AS jobs FROM code_breakage_spans LEFT JOIN code_breakage_affected_jobs ON code_breakage_affected_jobs.cause = code_breakage_spans.cause_id WHERE cause_commit_index <= ? AND (resolved_commit_index IS NULL OR ? < resolved_commit_index) GROUP BY code_breakage_spans.cause_id) foo JOIN code_breakage_cause ON foo.cause_id = code_breakage_cause.id"


listFlat1 :: (ToField b, FromField a) =>
     Query
  -> DbHelpers.DbConnectionData
  -> b
  -> IO [a]
listFlat1 sql conn_data t = do
  conn <- DbHelpers.get_connection conn_data
  map (\(Only x) -> x) <$> query conn sql (Only t)


listFlat1X :: (ToField b, FromField a) =>
     Query
  -> b
  -> DbIO [a]
listFlat1X sql t = do
  conn <- ask
  liftIO $ map (\(Only x) -> x) <$> query conn sql (Only t)


listFlat :: FromField a =>
     Query
  -> DbIO [a]
listFlat sql = do
  conn <- ask
  liftIO $ map (\(Only x) -> x) <$> query_ conn sql


data TagUsage = TagUsage {
    _tag           :: Text
  , _pattern_count :: Integer
  , _build_count   :: Integer
  } deriving (Generic, FromRow)

instance ToJSON TagUsage where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiTagsHistogram :: DbIO [TagUsage]
apiTagsHistogram = runQuery
  "SELECT tag, COUNT(*) AS pattern_count, SUM(matching_build_count)::bigint AS build_matches FROM pattern_tags LEFT JOIN pattern_frequency_summary ON pattern_frequency_summary.id = pattern_tags.pattern GROUP BY tag ORDER BY pattern_count DESC, build_matches DESC;"


data MasterCommitAndSourcePr = MasterCommitAndSourcePr {
    _sha1      :: Builds.RawCommit
  , _pr_number :: Builds.PullRequestNumber
  } deriving (Generic, FromRow)

instance ToJSON MasterCommitAndSourcePr where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | Gets Pull Request numbers of the commits that have been
-- implicated in master branch breakages
getImplicatedMasterCommitPullRequests :: DbIO [MasterCommitAndSourcePr]
getImplicatedMasterCommitPullRequests = runQuery
  "SELECT cause_sha1, github_pr_number FROM known_breakage_summaries_sans_impact WHERE github_pr_number IS NOT NULL ORDER BY cause_commit_index DESC;"


apiAutocompleteTags :: Text -> DbIO [Text]
apiAutocompleteTags = listFlat1X
  "SELECT tag FROM (SELECT tag, COUNT(*) AS freq FROM pattern_tags GROUP BY tag ORDER BY freq DESC, tag ASC) foo WHERE tag ILIKE CONCAT(?,'%');"


apiAutocompleteSteps :: Text -> DbIO [Text]
apiAutocompleteSteps = listFlat1X
  "SELECT name FROM (SELECT name, COUNT(*) AS freq FROM build_steps_deduped_mitigation where name IS NOT NULL GROUP BY name ORDER BY freq DESC, name ASC) foo WHERE name ILIKE CONCAT(?,'%');"


apiListSteps :: DbIO [Text]
apiListSteps = listFlat
  "SELECT name FROM build_steps_deduped_mitigation WHERE name IS NOT NULL GROUP BY name ORDER BY COUNT(*) DESC, name ASC;"


apiAutocompleteBranches :: Text -> DbIO [Text]
apiAutocompleteBranches = listFlat1X
  "SELECT branch FROM global_builds WHERE branch ILIKE CONCAT(?,'%') GROUP BY branch ORDER BY COUNT(*) DESC;"


-- Not used yet
apiListBranches :: DbIO [Text]
apiListBranches = listFlat
  "SELECT branch, COUNT(*) AS count FROM global_builds WHERE branch != '' GROUP BY branch ORDER BY count DESC;"


instance FromRow CommitBuilds.CommitBuild where
  fromRow = do
    step_name <- field
    match_id <- field
    buildnum <- field
    vcs_rev <- field
    queuedat <- field
    jobname <- field
    branch <- field
    patt <- field
    line_number <- field
    line_count <- field
    line_text <- field
    span_start <- field
    span_end <- field
    specificity <- field
    universal_build <- field
    provider_id <- field
    build_namespace <- field
    succeeded <- field
    ci_label <- field
    ci_icon_url <- field
    started_at <- field
    finished_at <- field

    let provider_obj = Builds.CiProvider
          ci_icon_url
          ci_label

        universal_build_obj = Builds.UniversalBuild
          wrapped_build_num
          provider_id
          build_namespace
          succeeded
          wrapped_commit

        parent_build_obj = Builds.StorableBuild
          (DbHelpers.WithId universal_build universal_build_obj)
          build_obj

        wrapped_commit = Builds.RawCommit vcs_rev
        wrapped_build_num = Builds.NewBuildNumber buildnum

        build_obj = Builds.NewBuild
          wrapped_build_num
          wrapped_commit
          queuedat
          jobname
          branch
          started_at
          finished_at

        match_obj = MatchOccurrences.MatchOccurrencesForBuild
          step_name
          (ScanPatterns.PatternId patt)
          (MatchOccurrences.MatchId match_id)
          line_number
          line_count
          line_text
          span_start
          span_end
          specificity

    return $ CommitBuilds.NewCommitBuild
        parent_build_obj
        match_obj
        (DbHelpers.WithId provider_id provider_obj)


getRevisionBuilds ::
     GitRev.GitSha1
  -> DbIO [CommitBuilds.CommitBuild]
getRevisionBuilds git_revision = do
  conn <- ask
  liftIO $ query conn sql $ Only $ GitRev.sha1 git_revision

  where
    sql = "SELECT step_name, match_id, build, vcs_revision, queued_at, job_name, branch, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, universal_build, provider, build_namespace, succeeded, label, icon_url, started_at, finished_at FROM best_pattern_match_augmented_builds JOIN ci_providers ON ci_providers.id = best_pattern_match_augmented_builds.provider WHERE vcs_revision = ?;"


getMasterCommits ::
     Connection
  -> Pagination.ParentOffsetMode
  -> IO (Either Text (WeeklyStats.InclusiveNumericBounds Int64, [BuildResults.IndexedRichCommit]))
getMasterCommits conn parent_offset_mode =

  case parent_offset_mode of
    Pagination.CommitIndices bounds@(WeeklyStats.InclusiveNumericBounds minbound maxbound) -> do

      rows <- liftIO $ query conn sql_commit_id_bounds (minbound, maxbound)
      let mapped_rows = map f rows
      return $ pure (bounds, mapped_rows)

    Pagination.FixedAndOffset (Pagination.OffsetLimit offset_mode commit_count) -> runExceptT $ do
      latest_id <- ExceptT $ case offset_mode of

        Pagination.Count offset_count -> do
          xs <- query conn sql_first_commit_id $ Only offset_count
          return $ maybeToEither "No master commits!" $ Safe.headMay $ map (\(Only x) -> x) xs

        Pagination.Commit (Builds.RawCommit sha1) -> do
          xs <- query conn sql_associated_commit_id $ Only sha1
          return $ maybeToEither (T.unwords ["No commit with sha1", sha1]) $
            Safe.headMay $ map (\(Only x) -> x) xs

      rows <- liftIO $ query conn sql_commit_id_and_offset (latest_id :: Int64, commit_count)

      let mapped_rows = map f rows
          maybe_first_commit_index = DbHelpers.db_id <$> Safe.lastMay mapped_rows

      first_commit_index <- except $ maybeToEither "No commits found!" maybe_first_commit_index

      return (WeeklyStats.InclusiveNumericBounds first_commit_index latest_id, mapped_rows)

  where
    f (commit_id, commit_sha1, commit_number, maybe_pr_number, maybe_message, maybe_tree_sha1, maybe_author_name, maybe_author_email, maybe_author_date, maybe_committer_name, maybe_committer_email, maybe_committer_date) =
      DbHelpers.WithId commit_id $ BuildResults.CommitAndMetadata
        wrapped_sha1
        maybe_metadata
        commit_number
        maybe_pr_number
      where
        wrapped_sha1 = Builds.RawCommit commit_sha1
        maybe_metadata = Commits.CommitMetadata wrapped_sha1 <$>
          maybe_message <*>
          maybe_tree_sha1 <*>
          maybe_author_name <*>
          maybe_author_email <*>
          maybe_author_date <*>
          maybe_committer_name <*>
          maybe_committer_email <*>
          maybe_committer_date

    sql_first_commit_id = "SELECT id FROM ordered_master_commits ORDER BY id DESC LIMIT 1 OFFSET ?"
    sql_associated_commit_id = "SELECT id FROM ordered_master_commits WHERE sha1 = ?"

    sql_commit_id_and_offset = "SELECT master_ordered_commits_with_metadata.id, master_ordered_commits_with_metadata.sha1, master_ordered_commits_with_metadata.commit_number, master_ordered_commits_with_metadata.github_pr_number, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date FROM master_ordered_commits_with_metadata WHERE id <= ? ORDER BY id DESC LIMIT ?"

    sql_commit_id_bounds = "SELECT master_ordered_commits_with_metadata.id, master_ordered_commits_with_metadata.sha1, master_ordered_commits_with_metadata.commit_number, master_ordered_commits_with_metadata.github_pr_number, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date FROM master_ordered_commits_with_metadata WHERE id >= ? AND id <= ? ORDER BY id DESC"


data NonannotatedBuildBreakages = NonannotatedBuildBreakages {
    _detected_breakages :: BuildResults.DetectedBreakageModes
  , _universal_build    :: DbHelpers.WithTypedId Builds.UniversalBuildId Builds.UniversalBuild
  } deriving Generic

instance ToJSON NonannotatedBuildBreakages where
  toJSON = genericToJSON JsonUtils.dropUnderscore


instance FromRow NonannotatedBuildBreakages where
  fromRow = do
    detected_directional_breakages <- fromRow

    universal_build_id <- field

    universal_build_obj <- fromRow

    let wrapped_universal_build = DbHelpers.WithTypedId
          (Builds.UniversalBuildId universal_build_id)
          universal_build_obj

    return $ NonannotatedBuildBreakages
      detected_directional_breakages
      wrapped_universal_build



data CommitBreakageRegionCounts = CommitBreakageRegionCounts {
    _commit            :: DbHelpers.WithId Builds.RawCommit
  , _only_longitudinal :: Int64
  , _only_lateral      :: Int64
  , _both              :: Int64
  } deriving Generic

instance ToJSON CommitBreakageRegionCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromRow CommitBreakageRegionCounts where
  fromRow = do
    commit_with_id <- fromRow
    num1 <- field
    num2 <- field
    num3 <- field

    return $ CommitBreakageRegionCounts
      commit_with_id
      num1
      num2
      num3


apiLeftoverCodeBreakagesByCommit :: DbIO [CommitBreakageRegionCounts]
apiLeftoverCodeBreakagesByCommit = runQuery
  "SELECT id, vcs_revision, only_longitudinal_breakages, only_lateral_breakages, both_breakages FROM master_unmarked_breakage_regions_by_commit;"



apiLeftoverDetectedCodeBreakages :: DbIO [NonannotatedBuildBreakages]
apiLeftoverDetectedCodeBreakages = runQuery
  "SELECT master_detected_breakages_without_annotations.contiguous_group_position, master_detected_breakages_without_annotations.contiguous_group_index, master_detected_breakages_without_annotations.contiguous_start_commit_index, master_detected_breakages_without_annotations.contiguous_end_commit_index, master_detected_breakages_without_annotations.contiguous_length, master_detected_breakages_without_annotations.cluster_id, master_detected_breakages_without_annotations.cluster_member_count, builds_join_steps.universal_build, builds_join_steps.build_num, builds_join_steps.provider, builds_join_steps.build_namespace, builds_join_steps.succeeded, builds_join_steps.vcs_revision FROM master_detected_breakages_without_annotations JOIN builds_join_steps ON master_detected_breakages_without_annotations.universal_build = builds_join_steps.universal_build;"


instance FromRow BuildResults.SimpleBuildStatus where
  fromRow = do
    sha1 <- field
    succeeded <- field
    is_idiopathic <- field
    is_flaky <-field
    is_timeout <- field
    is_matched <- field
    is_known_broken <- field
    build_num <- field
    queued_at <- field
    job_name <- field
    branch <- field
    step_name <- field
    pattern_id <- field
    match_id <- field
    line_number <- field
    line_count <- field
    line_text <- field
    span_start <- field
    span_end <- field
    specificity <- field
    is_serially_isolated <- field

    maybe_started_at <- field
    maybe_finished_at <- field

    universal_build_id <- field
    provider_id <- field
    build_namespace <- field

    detected_directional_breakages <- fromRow

    let
      failure_mode
        | succeeded = BuildResults.Success
        | is_idiopathic = BuildResults.NoLog
        | is_timeout = BuildResults.FailedStep step_name BuildResults.Timeout
        | is_matched = BuildResults.FailedStep step_name $ BuildResults.PatternMatch match_obj
        | otherwise = BuildResults.FailedStep step_name BuildResults.NoMatch


      wrapped_build_num = Builds.NewBuildNumber build_num
      wrapped_commit = Builds.RawCommit sha1

      build_obj = Builds.NewBuild
        wrapped_build_num
        wrapped_commit
        queued_at
        job_name
        branch
        maybe_started_at
        maybe_finished_at

      match_obj = MatchOccurrences.MatchOccurrencesForBuild
        step_name
        (ScanPatterns.PatternId pattern_id)
        (MatchOccurrences.MatchId match_id)
        line_number
        line_count
        line_text
        span_start
        span_end
        specificity

      ubuild_obj = DbHelpers.WithId universal_build_id $
        Builds.UniversalBuild
          wrapped_build_num
          provider_id
          build_namespace
          succeeded
          wrapped_commit

    return $ BuildResults.SimpleBuildStatus
      build_obj
      failure_mode
      is_flaky
      is_known_broken
      detected_directional_breakages
      is_serially_isolated
      ubuild_obj


refreshCachedMasterGrid :: Connection -> IO ()
refreshCachedMasterGrid conn = do
  MyUtils.debugStr "Refreshing view..."

  (execution_time, _) <- MyUtils.timeThisFloat $ execute_ conn "REFRESH MATERIALIZED VIEW CONCURRENTLY master_failures_raw_causes_mview;"

  execute conn "INSERT INTO lambda_logging.materialized_view_refresh_events (view_name, execution_duration_seconds, event_source) VALUES (?, ?, ?);" ("master_failures_raw_causes_mview" :: Text, execution_time, "frontend" :: Text)

  MyUtils.debugStr "View refreshed."


getLastCachedMasterGridRefreshTime :: Connection -> IO (UTCTime, Text)
getLastCachedMasterGridRefreshTime conn = do
  [tuple] <- query conn sql (Only ("master_failures_raw_causes_mview" :: String))
  return tuple
  where

    sql = "SELECT timestamp, event_source FROM lambda_logging.materialized_view_refresh_events WHERE view_name = ? ORDER BY timestamp DESC LIMIT 1;"


-- | Gets last N commits in one query,
-- then gets the list of jobs that apply to those commits,
-- then gets the associated builds
apiMasterBuilds ::
     DbHelpers.DbConnectionData -- ^ TODO get rid of this
  -> Pagination.ParentOffsetMode
  -> DbIO (Either Text BuildResults.MasterBuildsResponse)
apiMasterBuilds _mview_connection_data offset_limit = do

  (code_breakages_time, code_breakage_ranges) <- MyUtils.timeThisFloat apiAnnotatedCodeBreakages

  conn <- ask
  liftIO $ runExceptT $ do

    (commits_list_time, (commit_id_bounds, master_commits)) <- MyUtils.timeThisFloat $
      ExceptT $ getMasterCommits conn offset_limit

    let query_bounds = (WeeklyStats.min_bound commit_id_bounds, WeeklyStats.max_bound commit_id_bounds)

    (builds_list_time, completed_builds) <- MyUtils.timeThisFloat $
      liftIO $ query conn builds_list_sql query_bounds

    last_update_time <- liftIO $ getLastCachedMasterGridRefreshTime conn


    let failed_builds = filter (not . BuildResults.isSuccess . BuildResults._failure_mode) completed_builds
        job_names = Set.fromList $ map (Builds.job_name . BuildResults._build) failed_builds

    return $ BuildResults.MasterBuildsResponse
      job_names
      master_commits
      completed_builds
      code_breakage_ranges $
        BuildResults.DbMasterBuildsBenchmarks
          builds_list_time
          commits_list_time
          code_breakages_time
          last_update_time

  where
    builds_list_sql = "SELECT sha1, succeeded, is_idiopathic, is_flaky, is_timeout, is_matched, is_known_broken, build_num, queued_at, job_name, branch, step_name, pattern_id, match_id, line_number, line_count, line_text, span_start, span_end, specificity, is_serially_isolated, started_at, finished_at, global_build, provider, build_namespace, contiguous_run_count, contiguous_group_index, contiguous_start_commit_index, contiguous_end_commit_index, contiguous_length, cluster_id, cluster_member_count FROM master_failures_raw_causes_mview WHERE commit_index >= ? AND commit_index <= ?;"


data BreakageDateRangeSimple = BreakageDateRangeSimple {
    _pr                          :: Maybe Int
  , _foreshadowed_by_pr_failures :: Bool
  , _start                       :: UTCTime
  , _end                         :: UTCTime
  } deriving (Generic, FromRow)

instance ToJSON BreakageDateRangeSimple where
  toJSON = genericToJSON JsonUtils.dropUnderscore


masterCommitsGranular :: DbIO [BreakageDateRangeSimple]
masterCommitsGranular = runQuery
  "SELECT github_pr_number, foreshadowed_by_pr_failures, start_date, end_date FROM code_breakage_nonoverlapping_spans_dated;"


apiDetectedCodeBreakages :: DbIO [BuildResults.DetectedBreakageSpan]
apiDetectedCodeBreakages = runQuery
  "SELECT first_commit_id, jobs, job_count, min_run_length, max_run_length, modal_run_length, min_last_commit_id, max_last_commit_id, modal_last_commit_id, first_commit, min_last_commit, max_last_commit, modal_last_commit FROM master_contiguous_failure_blocks_with_commits ORDER BY first_commit_id DESC"


apiListFailureModes :: DbIO [DbHelpers.WithId BuildResults.MasterFailureModeDetails]
apiListFailureModes = runQuery
  "SELECT id, label, revertible FROM master_failure_modes ORDER BY id"


apiAnnotatedCodeBreakages :: DbIO [BuildResults.BreakageSpan Text ()]
apiAnnotatedCodeBreakages = runQuery
  "SELECT cause_id, cause_commit_index, cause_sha1, description, failure_mode_reporter, failure_mode_reported_at, failure_mode_id, cause_reporter, cause_reported_at, cause_jobs, breakage_commit_author, breakage_commit_message, breakage_commit_date, resolution_id, resolved_commit_index, resolution_sha1, resolution_reporter, resolution_reported_at, resolution_commit_author, resolution_commit_message, resolution_commit_date, spanned_commit_count, commit_timespan_seconds FROM known_breakage_summaries_sans_impact ORDER BY cause_commit_index DESC;"


-- | Identical to above except for addition of:
-- * downstream_broken_commit_count
-- * failed_downstream_build_count
-- * github_pr_number
-- * github_pr_head_commit
-- * foreshadowed_broken_jobs_delimited
apiAnnotatedCodeBreakagesWithImpact :: DbIO [BuildResults.BreakageSpan Text BuildResults.BreakageImpactStats]
apiAnnotatedCodeBreakagesWithImpact = runQuery
  "SELECT cause_id, cause_commit_index, cause_sha1, description, failure_mode_reporter, failure_mode_reported_at, failure_mode_id, cause_reporter, cause_reported_at, cause_jobs, breakage_commit_author, breakage_commit_message, breakage_commit_date, resolution_id, resolved_commit_index, resolution_sha1, resolution_reporter, resolution_reported_at, resolution_commit_author, resolution_commit_message, resolution_commit_date, spanned_commit_count, commit_timespan_seconds, downstream_broken_commit_count, failed_downstream_build_count, github_pr_number, github_pr_head_commit, foreshadowed_broken_jobs_delimited FROM known_breakage_summaries ORDER BY cause_commit_index DESC;"


apiBreakageAuthorStats :: DbIO [BuildResults.BreakageAuthorStats]
apiBreakageAuthorStats = runQuery
  "SELECT breakage_commit_author, distinct_breakage_count, cumulative_breakage_duration_seconds, cumulative_downstream_affected_commits, cumulative_spanned_master_commits FROM upstream_breakage_author_stats ORDER BY distinct_breakage_count DESC;"


apiBrokenCommitsWithoutMetadata :: DbIO [Builds.RawCommit]
apiBrokenCommitsWithoutMetadata = runQuery
  "SELECT vcs_revision FROM broken_commits_without_metadata;"


getLatestMasterCommitWithMetadata :: DbIO (Either Text Builds.RawCommit)
getLatestMasterCommitWithMetadata = do
  conn <- ask
  liftIO $ do
    rows <- query_ conn sql
    return $ maybeToEither "No commit has metdata" $ Safe.headMay $ map (\(Only x) -> Builds.RawCommit x) rows
  where
    sql = "SELECT ordered_master_commits.sha1 FROM ordered_master_commits LEFT JOIN commit_metadata ON ordered_master_commits.sha1 = commit_metadata.sha1 WHERE commit_metadata.sha1 IS NOT NULL ORDER BY ordered_master_commits.id DESC LIMIT 1"


data ScanTestResponse = ScanTestResponse {
    _total_line_count :: Int
  , _matches          :: [ScanPatterns.ScanMatch]
  } deriving Generic

instance ToJSON ScanTestResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiNewPatternTest ::
     Builds.UniversalBuildId
  -> ScanPatterns.Pattern
  -> DbIO (Either String ScanTestResponse)
apiNewPatternTest universal_build_id new_pattern = do
  conn <- ask
  storable_build <- liftIO $ SqlRead.getGlobalBuild conn universal_build_id
  let provider_build_number = Builds.build_id $ Builds.build_record storable_build

  -- TODO consolidate with Scanning.scan_log
  -- TODO SqlRead.readLog should accept a universal build number
  maybe_console_log <- liftIO $ SqlRead.readLog conn universal_build_id

  return $ case maybe_console_log of
    Just console_log -> let mylines = LT.lines console_log
      in Right $ ScanTestResponse (length mylines) $
           Maybe.mapMaybe apply_pattern $ zip [0::Int ..] $
             map LT.stripEnd mylines
    Nothing -> Left $ unwords [
        "No log found for build number"
      , show provider_build_number
      ]

  where
    apply_pattern :: (Int, LT.Text) -> Maybe ScanPatterns.ScanMatch
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


apiSummaryStats :: DbIO SummaryStats
apiSummaryStats = do
 conn <- ask
 liftIO $ do
  [Only build_count] <- query_ conn "SELECT COUNT(*) FROM global_builds"
  [Only visited_count] <- query_ conn "SELECT COUNT(*) FROM build_steps_deduped_mitigation"
  [Only explained_count] <- query_ conn "SELECT COUNT(*) FROM build_steps_deduped_mitigation WHERE name IS NOT NULL"
  [Only timeout_count] <- query_ conn "SELECT COUNT(*) FROM build_steps_deduped_mitigation WHERE is_timeout"
  [Only matched_steps_count] <- query_ conn "SELECT COUNT(*) FROM (SELECT build_step FROM public.matches_distinct GROUP BY build_step) x"
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
    PatternRecord a b c d e f g (DbHelpers.splitAggText h) (DbHelpers.splitAggText i) j k


-- | Returns zero or one pattern.
apiSinglePattern :: ScanPatterns.PatternId -> DbIO [PatternRecord]
apiSinglePattern (ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ fmap make_pattern_records $ query conn sql $ Only pattern_id
  where
    sql = "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary WHERE id = ?;"


apiPatterns :: DbIO [PatternRecord]
apiPatterns = make_pattern_records <$> runQuery
  "SELECT id, regex, expression, description, matching_build_count, most_recent, earliest, tags, steps, specificity, CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM pattern_frequency_summary ORDER BY most_recent DESC NULLS LAST;"


-- | For the purpose of database upgrades
dumpPresumedStableBranches :: DbIO [Text]
dumpPresumedStableBranches = listFlat
  "SELECT branch FROM presumed_stable_branches ORDER BY branch;"


-- | For the purpose of database upgrades
dumpPatterns :: DbIO [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
dumpPatterns = map f <$> runQuery
  "SELECT author, created, id, regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end FROM patterns_augmented ORDER BY id;"
  where
    split_texts = sort . map T.pack . DbHelpers.splitAggText

    f (author, created, pattern_id, is_regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end) =
      DbHelpers.WithAuthorship author created $ wrapPattern pattern_id is_regex expression has_nondeterministic_values description
        (split_texts tags)
        (split_texts steps)
        specificity
        is_retired
        lines_from_end


-- | Note that this SQL is from decomposing the "pattern_frequency_summary" and "aggregated_build_matches" view
-- to parameterize the latter by branch.
--
-- TODO: Should just pair this with commits from the master branch
-- instead of relying on the branch name (which is not available from
-- GitHub notifications).
--
-- For more signal, "dummy" commits should be created with
-- "git commit --allow-empty" and submitted to CI. These will have
-- the same "tree" SHA1 as master commits, and can be JOINed on that.
apiPatternsBranchFiltered :: [Text] -> DbIO [PatternRecord]
apiPatternsBranchFiltered branches = do
  conn <- ask
  liftIO $ fmap make_pattern_records $ query conn sql $ Only $ In branches

  where
    sql = "SELECT patterns_augmented.id, patterns_augmented.regex, patterns_augmented.expression, patterns_augmented.description, COALESCE(aggregated_build_matches.matching_build_count, 0::int) AS matching_build_count, aggregated_build_matches.most_recent, aggregated_build_matches.earliest, patterns_augmented.tags, patterns_augmented.steps, patterns_augmented.specificity, CAST((patterns_augmented.scanned_count * 100 / patterns_augmented.total_scanned_builds) AS DECIMAL(6, 1)) AS percent_scanned FROM patterns_augmented LEFT JOIN (SELECT best_pattern_match_for_builds.pattern_id AS pat, count(best_pattern_match_for_builds.build) AS matching_build_count, max(global_builds.queued_at) AS most_recent, min(global_builds.queued_at) AS earliest FROM best_pattern_match_for_builds JOIN global_builds ON global_builds.build_number = best_pattern_match_for_builds.build WHERE global_builds.branch IN ? GROUP BY best_pattern_match_for_builds.pattern_id ) aggregated_build_matches ON patterns_augmented.id = aggregated_build_matches.pat ORDER BY matching_build_count DESC"


getPresumedStableBranches :: DbIO [Text]
getPresumedStableBranches = listFlat
  "SELECT branch FROM presumed_stable_branches;"


apiPatternsPresumedStableBranches :: DbIO [PatternRecord]
apiPatternsPresumedStableBranches = do
  branches <- getPresumedStableBranches
  apiPatternsBranchFiltered branches


data PatternOccurrence = NewPatternOccurrence {
    _build_number       :: Builds.BuildNumber
  , _pattern_id         :: ScanPatterns.PatternId
  , _match_id           :: MatchOccurrences.MatchId
  , _vcs_revision       :: Builds.RawCommit
  , _queued_at          :: UTCTime
  , _job_name           :: Text
  , _branch             :: Maybe Text
  , _build_step         :: Text
  , _line_number        :: Int
  , _line_count         :: Int
  , _line_text          :: LT.Text
  , _span_start         :: Int
  , _span_end           :: Int
  , _universal_build_id :: Builds.UniversalBuildId
  } deriving Generic

instance ToJSON PatternOccurrence where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getBuildPatternMatches ::
     Builds.UniversalBuildId
  -> DbIO [MatchOccurrences.MatchOccurrencesForBuild]
getBuildPatternMatches (Builds.UniversalBuildId build_id) = do
  conn <- ask
  liftIO $ query conn sql $ Only build_id
  where
    sql = "SELECT step_name, pattern, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, specificity FROM matches_with_log_metadata JOIN build_steps_deduped_mitigation ON matches_with_log_metadata.build_step = build_steps_deduped_mitigation.id JOIN patterns_augmented ON patterns_augmented.id = matches_with_log_metadata.pattern WHERE build_steps_deduped_mitigation.universal_build = ? ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"


data StorageStats = StorageStats {
    _total_lines :: Integer
  , _total_bytes :: Integer
  , _log_count   :: Integer
  } deriving (Generic, FromRow)

instance ToJSON StorageStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | FIXME partial head
apiStorageStats :: DbIO StorageStats
apiStorageStats = head <$> runQuery
  "SELECT SUM(line_count) AS total_lines, SUM(byte_count) AS total_bytes, COUNT(*) log_count FROM log_metadata"


pattern_occurrence_txform pattern_id = f
  where
    -- TODO consolidate this transformation with "getPatternMatches"
    f (buildnum, stepname, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, universal_build_id) =
     NewPatternOccurrence
      buildnum
      pattern_id
      match_id
      (Builds.RawCommit vcs_revision)
      queued_at job_name
      branch
      stepname
      line_number
      line_count
      line_text
      span_start
      span_end
      universal_build_id


-- | Limit is arbitrary
getBestPatternMatches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getBestPatternMatches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (pattern_occurrence_txform pat) <$> query conn sql (Only pattern_id)

  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, universal_build FROM best_pattern_match_augmented_builds WHERE pattern_id = ? LIMIT 100;"


getBestPatternMatchesWhitelistedBranches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getBestPatternMatchesWhitelistedBranches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (pattern_occurrence_txform pat) <$> query conn sql (Only pattern_id)
  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, universal_build FROM best_pattern_match_augmented_builds WHERE pattern_id = ? AND branch IN (SELECT branch from presumed_stable_branches);"


getPostedGithubStatus ::
     Connection
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Maybe (Text, Text))
getPostedGithubStatus conn (DbHelpers.OwnerAndRepo project repo) (Builds.RawCommit sha1) = do

  xs <- query conn sql (sha1, project, repo)
  return $ Safe.headMay xs

  where
    sql = "SELECT state, description FROM created_github_statuses WHERE sha1 = ? AND project = ? AND repo = ? ORDER BY id DESC LIMIT 1;"


-- | This should produce one or zero results.
-- We use a list instead of a Maybe so that
-- the javascript table renderer code can be reused
-- for multi-item lists.
getBestBuildMatch :: Builds.UniversalBuildId -> DbIO [PatternOccurrence]
getBestBuildMatch ubuild_id@(Builds.UniversalBuildId build_id) = do

  conn <- ask
  liftIO $ map f <$> query conn sql (Only build_id)

  where
    f (pattern_id, build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch) = pattern_occurrence_txform (ScanPatterns.PatternId pattern_id) (build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, ubuild_id)

    sql = "SELECT pattern_id, build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch FROM best_pattern_match_augmented_builds WHERE universal_build = ?;"


data LogContext = LogContext {
    _match_info         :: ScanPatterns.MatchDetails
  , _log_lines          :: [(Int, LT.Text)]
  , _build_number       :: Builds.BuildNumber
  , _universal_build_id :: Builds.UniversalBuildId
  } deriving Generic

instance ToJSON LogContext where
  toJSON = genericToJSON JsonUtils.dropUnderscore


logContextFunc ::
     MatchOccurrences.MatchId
  -> Int
  -> DbIO (Either Text LogContext)
logContextFunc (MatchOccurrences.MatchId match_id) context_linecount = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only match_id
    let maybe_first_row = Safe.headMay xs

    runExceptT $ do
      first_row <- except $ maybeToEither (T.pack $ unwords ["Match ID", show match_id, "not found"]) maybe_first_row

      let (build_num, line_number, span_start, span_end, line_text, universal_build) = first_row
          match_info = ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end
          wrapped_build_num = Builds.NewBuildNumber build_num

      maybe_log <- liftIO $ SqlRead.readLog conn universal_build
      console_log <- except $ maybeToEither "log not in database" maybe_log

      let log_lines = LT.lines console_log

          first_context_line = max 0 $ line_number - context_linecount

          tuples = zip [first_context_line..] $ take (2 * context_linecount + 1) $ drop first_context_line log_lines

      return $ LogContext
        match_info
        tuples
        wrapped_build_num
        universal_build

  where
    sql = "SELECT build_num, line_number, span_start, span_end, line_text, universal_build FROM matches_with_log_metadata WHERE id = ?"


getPatternMatches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getPatternMatches pattern_id =
  map f <$> get_pattern_occurrence_rows pattern_id
  where
    f (build_obj, stepname, line_count, match_id, match_details, global_build_id) =
      NewPatternOccurrence
        buildnum
        pattern_id
        match_id
        vcs_rev
        queued_at
        job_name
        branch
        stepname
        line_number
        line_count
        line_text
        start
        end
        global_build_id
      where
        (Builds.NewBuild buildnum vcs_rev queued_at job_name branch _ _) = build_obj
        (ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end)) = match_details


get_pattern_occurrence_rows ::
     ScanPatterns.PatternId
  -> DbIO [(Builds.Build, Text, Int, MatchOccurrences.MatchId, ScanPatterns.MatchDetails, Builds.UniversalBuildId)]
get_pattern_occurrence_rows (ScanPatterns.PatternId pattern_id) = do

  conn <- ask
  liftIO $ fmap (map f) $ query conn sql $ Only pattern_id

  where
    f (buildnum, stepname, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, global_build_num, maybe_started_at, maybe_finished_at) =
      (build_obj, stepname, line_count, MatchOccurrences.MatchId match_id, match_details, Builds.UniversalBuildId global_build_num)
      where
        build_obj = Builds.NewBuild
          (Builds.NewBuildNumber buildnum)
          (Builds.RawCommit vcs_revision)
          queued_at
          job_name
          branch
          maybe_started_at
          maybe_finished_at

        match_details = ScanPatterns.NewMatchDetails
          line_text
          line_number $
            ScanPatterns.NewMatchSpan span_start span_end

    sql = "SELECT global_builds.build_number, step_name, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, global_builds.vcs_revision, queued_at, job_name, branch, global_build_num, global_builds.started_at, global_builds.finished_at FROM matches_with_log_metadata JOIN global_builds ON matches_with_log_metadata.universal_build = global_builds.global_build_num WHERE pattern = ?;"
