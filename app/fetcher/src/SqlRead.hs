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
import           Data.Bifunctor                       (first)
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
import           Database.PostgreSQL.Simple.FromRow   (field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           GHC.Generics
import           GHC.Int                              (Int64)
import qualified Network.OAuth.OAuth2                 as OAuth2
import qualified Safe

import qualified BuildResults
import qualified Builds
import qualified CommitBuilds
import qualified Commits
import qualified DbHelpers
import qualified GithubApiFetch
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Pagination
import qualified PostedStatuses
import qualified ScanPatterns
import qualified ScanUtils
import qualified WebApi
import qualified WeeklyStats


type DbIO a = ReaderT Connection IO a


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
    Just limit -> query conn sql (circleCIProviderId, limit)
    Nothing    -> query conn unlimited_sql (Only circleCIProviderId)
  return $ map f rows
  where
    -- TODO parameterize this function
    circleCIProviderId = 3 :: Int

    f (universal_build_id, provider_buildnum, provider_id, build_namespace, succeeded, sha1) = DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber provider_buildnum)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit sha1)

    sql = "SELECT universal_build_id, build_num, provider, build_namespace, succeeded, commit_sha1 FROM unvisited_builds WHERE provider = ? ORDER BY build_num DESC LIMIT ?;"

    unlimited_sql = "SELECT universal_build_id, build_num, provider, build_namespace, succeeded, commit_sha1 FROM unvisited_builds WHERE provider = ? ORDER BY build_num DESC;"


-- | XXX This is a partial function
getGlobalBuild ::
     Connection
  -> Builds.UniversalBuildId
  -> IO Builds.StorableBuild
getGlobalBuild conn (Builds.UniversalBuildId global_build_num) = do
  [x] <- query conn sql $ Only global_build_num
  return x
  where
    sql = "SELECT global_build_num, build_number, provider, build_namespace, succeeded, vcs_revision, queued_at, job_name, branch FROM global_builds WHERE global_build_num = ?;"


getRevisitableBuilds ::
     Connection
  -> IO [(Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
getRevisitableBuilds conn =
  map f <$> query_ conn sql
  where
    f (delimited_pattern_ids, step_id, step_name, universal_build_id, build_num, provider_id, build_namespace, succeeded, vcs_revision) =
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
      pattern_occurrences <- get_best_pattern_matches_whitelisted_branches test_failure_pattern_id
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
     DbHelpers.DbConnectionData
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
getNextMasterCommit conn_data (Builds.RawCommit current_git_revision) = do
  conn <- DbHelpers.get_connection conn_data
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
  "SELECT build, branch, global_build FROM unattributed_failed_builds ORDER BY build DESC;"


apiIdiopathicBuilds :: DbIO [WebApi.BuildBranchRecord]
apiIdiopathicBuilds = listBuilds
  "SELECT build, branch, global_build_num FROM idiopathic_build_failures ORDER BY build DESC;"


apiUnmatchedCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiUnmatchedCommitBuilds sha1 = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = "SELECT build, step_name, queued_at, job_name, unattributed_failed_builds.branch, builds_join_steps.universal_build, ci_providers.icon_url, ci_providers.label FROM unattributed_failed_builds JOIN builds_join_steps ON unattributed_failed_builds.global_build = builds_join_steps.universal_build JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ?"


apiIdiopathicCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiIdiopathicCommitBuilds sha1 = do
  conn <- ask
  liftIO $ map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, universal_build_id, provider_icon_url, provider_label) = WebApi.UnmatchedBuild (Builds.NewBuildNumber build) step_name queued_at job_name branch (Builds.UniversalBuildId universal_build_id) provider_icon_url provider_label
    sql = "SELECT build, step_name, queued_at, job_name, idiopathic_build_failures.branch, builds_join_steps.universal_build, ci_providers.icon_url, ci_providers.label FROM idiopathic_build_failures JOIN builds_join_steps ON idiopathic_build_failures.global_build_num = builds_join_steps.universal_build JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ?"


apiTimeoutCommitBuilds :: Text -> DbIO [WebApi.UnmatchedBuild]
apiTimeoutCommitBuilds sha1 = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = "SELECT build_num, step_name, queued_at, job_name, branch, universal_build, ci_providers.icon_url, ci_providers.label FROM builds_join_steps JOIN ci_providers ON builds_join_steps.provider = ci_providers.id WHERE vcs_revision = ? AND is_timeout;"


-- | Obtains the console log from database
readLog :: Connection -> Builds.UniversalBuildId -> IO (Maybe Text)
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
masterWeeklyFailureStats :: Int -> DbIO [WeeklyStats.MasterWeeklyStats]
masterWeeklyFailureStats week_count = do

  conn <- ask
  xs <- liftIO $ query conn sql $ Only week_count
  return $ reverse $ map f xs
  where
    sql = "SELECT commit_count, had_failure, had_idiopathic, had_timeout, had_known_broken, had_pattern_matched, had_flaky, failure_count::int, idiopathic_count::int, timeout_count::int, known_broken_count::int, pattern_matched_count::int, pattern_unmatched_count::int, flaky_count::int, earliest_commit_index, latest_commit_index, week FROM master_failures_weekly_aggregation ORDER BY week DESC LIMIT ? OFFSET 1"

    f (commit_count, had_failure, had_idiopathic, had_timeout, had_known_broken, had_pattern_matched, had_flaky, failure_count, idiopathic_count, timeout_count, known_broken_count, pattern_matched_count, pattern_unmatched_count, flaky_count, earliest_commit_index, latest_commit_index, week) =
      WeeklyStats.MasterWeeklyStats commit_count had_failure had_idiopathic had_timeout had_known_broken had_pattern_matched had_flaky failure_count idiopathic_count timeout_count known_broken_count pattern_matched_count pattern_unmatched_count flaky_count week $ WeeklyStats.InclusiveNumericBounds earliest_commit_index latest_commit_index


getLatestKnownMasterCommit :: Connection -> IO (Maybe Text)
getLatestKnownMasterCommit conn = do
  rows <- query_ conn sql
  return $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = "SELECT sha1 FROM ordered_master_commits ORDER BY id DESC LIMIT 1;"


findMasterAncestor ::
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
findMasterAncestor conn access_token owner_and_repo sha1 = do

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
    target_commit_index <- ExceptT $ get_master_commit_index conn sha1

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


apiAutocompleteTags :: Text -> DbIO [Text]
apiAutocompleteTags = listFlat1X
  "SELECT tag FROM (SELECT tag, COUNT(*) AS freq FROM pattern_tags GROUP BY tag ORDER BY freq DESC, tag ASC) foo WHERE tag ILIKE CONCAT(?,'%');"


apiAutocompleteSteps :: Text -> DbIO [Text]
apiAutocompleteSteps = listFlat1X
  "SELECT name FROM (SELECT name, COUNT(*) AS freq FROM build_steps where name IS NOT NULL GROUP BY name ORDER BY freq DESC, name ASC) foo WHERE name ILIKE CONCAT(?,'%');"


apiListSteps :: DbIO [Text]
apiListSteps = listFlat
  "SELECT name FROM build_steps WHERE name IS NOT NULL GROUP BY name ORDER BY COUNT(*) DESC, name ASC;"


apiAutocompleteBranches :: Text -> DbIO [Text]
apiAutocompleteBranches = listFlat1X
  "SELECT branch FROM global_builds WHERE branch ILIKE CONCAT(?,'%') GROUP BY branch ORDER BY COUNT(*) DESC;"


-- Not used yet
api_list_branches :: DbIO [Text]
api_list_branches = listFlat
  "SELECT branch, COUNT(*) AS count FROM global_builds WHERE branch != '' GROUP BY branch ORDER BY count DESC;"


getRevisionBuilds ::
     DbHelpers.DbConnectionData
  -> GitRev.GitSha1
  -> IO [CommitBuilds.CommitBuild]
getRevisionBuilds conn_data git_revision = do
  conn <- DbHelpers.get_connection conn_data
  fmap (map f) $ query conn sql $ Only $ GitRev.sha1 git_revision

  where
    f (step_name, match_id, buildnum, vcs_rev, queuedat, jobname, branch, patt, line_number, line_count, line_text, span_start, span_end, specificity, universal_build, provider_id, build_namespace, succeeded, ci_label, ci_icon_url) =
      CommitBuilds.NewCommitBuild
        parent_build_obj
        match_obj
        (DbHelpers.WithId provider_id provider_obj)
      where

        provider_obj = Builds.CiProvider
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

    sql = "SELECT step_name, match_id, build, vcs_revision, queued_at, job_name, branch, pattern_id, line_number, line_count, line_text, span_start, span_end, specificity, universal_build, provider, build_namespace, succeeded, label, icon_url FROM best_pattern_match_augmented_builds JOIN ci_providers ON ci_providers.id = best_pattern_match_augmented_builds.provider WHERE vcs_revision = ?;"


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
    f (commit_id, commit_sha1, maybe_message, maybe_tree_sha1, maybe_author_name, maybe_author_email, maybe_author_date, maybe_committer_name, maybe_committer_email, maybe_committer_date) =
      DbHelpers.WithId commit_id $ BuildResults.CommitAndMetadata
        wrapped_sha1
        maybe_metadata
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

    sql_commit_id_and_offset = "SELECT ordered_master_commits.id, ordered_master_commits.sha1, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date FROM ordered_master_commits LEFT JOIN commit_metadata ON commit_metadata.sha1 = ordered_master_commits.sha1 WHERE id <= ? ORDER BY id DESC LIMIT ?"

    sql_commit_id_bounds = "SELECT ordered_master_commits.id, ordered_master_commits.sha1, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date FROM ordered_master_commits LEFT JOIN commit_metadata ON commit_metadata.sha1 = ordered_master_commits.sha1 WHERE id >= ? AND id <= ? ORDER BY id DESC"


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
    contiguous_run_count <- field
    contiguous_group_index <- field
    contiguous_start_commit_index <- field
    contiguous_end_commit_index <- field
    contiguous_length <- field
    universal_build_id <- field
    provider_id <- field
    build_namespace <- field
    maybe_cluster_id <- field
    maybe_cluster_member_count <- field

    let
      failure_mode
        | succeeded = BuildResults.Success
        | is_idiopathic = BuildResults.NoLog
        | is_timeout = BuildResults.FailedStep step_name BuildResults.Timeout
        | is_matched = BuildResults.FailedStep step_name $ BuildResults.PatternMatch match_obj
        | otherwise = BuildResults.FailedStep step_name BuildResults.NoMatch

      maybe_contiguous_member = if is_serially_isolated
        then Nothing
        else Just $ BuildResults.ContiguousBreakageMember
          contiguous_run_count
          contiguous_group_index
          contiguous_start_commit_index
          contiguous_end_commit_index
          contiguous_length

      maybe_lateral_breakage = BuildResults.LateralBreakageMember
        <$> maybe_cluster_member_count
        <*> maybe_cluster_id

      wrapped_build_num = Builds.NewBuildNumber build_num
      wrapped_commit = Builds.RawCommit sha1

      build_obj = Builds.NewBuild
        wrapped_build_num
        wrapped_commit
        queued_at
        job_name
        branch

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
      maybe_contiguous_member
      maybe_lateral_breakage
      ubuild_obj


-- | Gets last N commits in one query,
-- then gets the list of jobs that apply to those commits,
-- then gets the associated builds
apiMasterBuilds ::
     Pagination.ParentOffsetMode
  -> DbIO (Either Text BuildResults.MasterBuildsResponse)
apiMasterBuilds offset_limit = do

  code_breakage_ranges <- apiAnnotatedCodeBreakages

  conn <- ask
  liftIO $ runExceptT $ do

    (commit_id_bounds, master_commits) <- ExceptT $ getMasterCommits conn offset_limit
    let query_bounds = (WeeklyStats.min_bound commit_id_bounds, WeeklyStats.max_bound commit_id_bounds)
    failed_builds <- liftIO $ query conn failures_sql query_bounds

    let job_names = Set.fromList $ map (Builds.job_name . BuildResults._build) failed_builds

    return $ BuildResults.MasterBuildsResponse
      job_names
      master_commits
      failed_builds
      code_breakage_ranges

  where
    failures_sql = "SELECT sha1, succeeded, is_idiopathic, is_flaky, is_timeout, is_matched, is_known_broken, build_num, queued_at, job_name, branch, step_name, pattern_id, match_id, line_number, line_count, line_text, span_start, span_end, specificity, is_serially_isolated, contiguous_run_count, contiguous_group_index, contiguous_start_commit_index, contiguous_end_commit_index, contiguous_length, global_build, provider, build_namespace, cluster_id, cluster_member_count FROM master_failures_raw_causes WHERE commit_index >= ? AND commit_index <= ?;"


apiDetectedCodeBreakages :: DbIO [BuildResults.DetectedBreakageSpan]
apiDetectedCodeBreakages = runQuery
  "SELECT first_commit_id, jobs, job_count, min_run_length, max_run_length, modal_run_length, min_last_commit_id, max_last_commit_id, modal_last_commit_id, first_commit, min_last_commit, max_last_commit, modal_last_commit FROM master_contiguous_failure_blocks_with_commits ORDER BY first_commit_id DESC"


apiListFailureModes :: DbIO [DbHelpers.WithId BuildResults.MasterFailureModeDetails]
apiListFailureModes = runQuery
  "SELECT id, label, revertible FROM master_failure_modes ORDER BY id"


apiAnnotatedCodeBreakages :: DbIO [BuildResults.BreakageSpan Text]
apiAnnotatedCodeBreakages = runQuery
  "SELECT cause_id, cause_commit_index, cause_sha1, description, failure_mode_reporter, failure_mode_reported_at, failure_mode_id, cause_reporter, cause_reported_at, cause_jobs, resolution_id, resolved_commit_index, resolution_sha1, resolution_reporter, resolution_reported_at, breakage_commit_author, breakage_commit_message, resolution_commit_author, resolution_commit_message, breakage_commit_date, resolution_commit_date FROM known_breakage_summaries ORDER BY cause_commit_index DESC;"


get_latest_master_commit_with_metadata :: DbIO (Either Text Builds.RawCommit)
get_latest_master_commit_with_metadata = do
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
     DbHelpers.DbConnectionData
  -> Builds.UniversalBuildId
  -> ScanPatterns.Pattern
  -> IO (Either String ScanTestResponse)
apiNewPatternTest conn_data universal_build_id new_pattern = do

  conn <- DbHelpers.get_connection conn_data

  storable_build <- SqlRead.getGlobalBuild conn universal_build_id
  let provider_build_number = Builds.build_id $ Builds.build_record storable_build

  -- TODO consolidate with Scanning.scan_log
  -- TODO SqlRead.readLog should accept a universal build number
  maybe_console_log <- SqlRead.readLog conn universal_build_id

  return $ case maybe_console_log of
    Just console_log -> Right $ ScanTestResponse (length $ T.lines console_log) $
      Maybe.mapMaybe apply_pattern $ zip [0::Int ..] $ map T.stripEnd $ T.lines console_log
    Nothing -> Left $ "No log found for build number " ++ show provider_build_number

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


apiSummaryStats :: DbIO SummaryStats
apiSummaryStats = do
 conn <- ask
 liftIO $ do
  [Only build_count] <- query_ conn "SELECT COUNT(*) FROM global_builds"
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
  , _branch             :: Text
  , _build_step         :: Text
  , _line_number        :: Int
  , _line_count         :: Int
  , _line_text          :: Text
  , _span_start         :: Int
  , _span_end           :: Int
  , _universal_build_id :: Builds.UniversalBuildId
  } deriving Generic

instance ToJSON PatternOccurrence where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getBuildPatternMatches :: Builds.UniversalBuildId -> DbIO [MatchOccurrences.MatchOccurrencesForBuild]
getBuildPatternMatches (Builds.UniversalBuildId build_id) = do
  conn <- ask
  liftIO $ query conn sql $ Only build_id
  where
    sql = "SELECT step_name, pattern, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, specificity FROM matches_with_log_metadata JOIN build_steps ON matches_with_log_metadata.build_step = build_steps.id JOIN patterns_augmented ON patterns_augmented.id = matches_with_log_metadata.pattern WHERE build_steps.universal_build = ? ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"


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
      queued_at
      job_name
      branch
      stepname
      line_number
      line_count
      line_text
      span_start
      span_end
      universal_build_id


get_best_pattern_matches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
get_best_pattern_matches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (pattern_occurrence_txform pat) <$> query conn sql (Only pattern_id)

  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, universal_build FROM best_pattern_match_augmented_builds WHERE pattern_id = ?;"


get_best_pattern_matches_whitelisted_branches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
get_best_pattern_matches_whitelisted_branches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (pattern_occurrence_txform pat) <$> query conn sql (Only pattern_id)
  where
    sql = "SELECT build, step_name, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, universal_build FROM best_pattern_match_augmented_builds WHERE pattern_id = ? AND branch IN (SELECT branch from presumed_stable_branches);"


get_posted_github_status ::
     DbHelpers.DbConnectionData
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Maybe (Text, Text))
get_posted_github_status conn_data (DbHelpers.OwnerAndRepo project repo) (Builds.RawCommit sha1) = do

  conn <- DbHelpers.get_connection conn_data

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
  , _log_lines          :: [(Int, Text)]
  , _build_number       :: Builds.BuildNumber
  , _universal_build_id :: Builds.UniversalBuildId
  } deriving Generic

instance ToJSON LogContext where
  toJSON = genericToJSON JsonUtils.dropUnderscore


logContextFunc ::
     DbHelpers.DbConnectionData
  -> MatchOccurrences.MatchId
  -> Int
  -> IO (Either Text LogContext)
logContextFunc connection_data (MatchOccurrences.MatchId match_id) context_linecount = do
  conn <- DbHelpers.get_connection connection_data

  xs <- query conn sql $ Only match_id
  let maybe_first_row = Safe.headMay xs

  runExceptT $ do
    first_row <- except $ maybeToEither (T.pack $ unwords ["Match ID", show match_id, "not found"]) maybe_first_row

    let (build_num, line_number, span_start, span_end, line_text, universal_build) = first_row
        match_info = ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end
        wrapped_build_num = Builds.NewBuildNumber build_num

    maybe_log <- liftIO $ SqlRead.readLog conn universal_build
    console_log <- except $ maybeToEither "log not in database" maybe_log

    let log_lines = T.lines console_log

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
    f (Builds.NewBuild buildnum vcs_rev queued_at job_name branch, stepname, line_count, match_id, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan start end), global_build_id) =
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


get_pattern_occurrence_rows ::
     ScanPatterns.PatternId
  -> DbIO [(Builds.Build, Text, Int, MatchOccurrences.MatchId, ScanPatterns.MatchDetails, Builds.UniversalBuildId)]
get_pattern_occurrence_rows (ScanPatterns.PatternId pattern_id) = do

  conn <- ask
  liftIO $ fmap (map f) $ query conn sql $ Only pattern_id

  where
    f (buildnum, stepname, match_id, line_number, line_count, line_text, span_start, span_end, vcs_revision, queued_at, job_name, branch, global_build_num) =
      (Builds.NewBuild (Builds.NewBuildNumber buildnum) (Builds.RawCommit vcs_revision) queued_at job_name branch, stepname, line_count, MatchOccurrences.MatchId match_id, ScanPatterns.NewMatchDetails line_text line_number $ ScanPatterns.NewMatchSpan span_start span_end, Builds.UniversalBuildId global_build_num)

    sql = "SELECT global_builds.build_number, step_name, matches_with_log_metadata.id, line_number, line_count, line_text, span_start, span_end, global_builds.vcs_revision, queued_at, job_name, branch, global_build_num FROM matches_with_log_metadata JOIN global_builds ON matches_with_log_metadata.universal_build = global_builds.global_build_num WHERE pattern = ?;"
