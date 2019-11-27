{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlRead where

import           Control.Exception                    (throwIO)
import           Control.Monad                        (forM, unless)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       except, runExceptT)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import           Data.Aeson
import qualified Data.ByteString.Char8                as BS
import           Data.Either.Utils                    (maybeToEither)
import           Data.List                            (partition, sort, sortOn)
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
import           Data.Time.LocalTime                  (TimeOfDay)
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
import qualified PostedComments
import qualified PostedStatuses
import qualified PostgresHelpers
import qualified QueryUtils                           as Q
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
    patterns_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "id"
        , "regex"
        , "expression"
        , "has_nondeterministic_values"
        , "description"
        , "specificity"
        , "is_retired"
        , "lines_from_end"
        ]
      , "FROM patterns ORDER BY description;"
      ]

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

    unlimited_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "universal_build_id"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "commit_sha1"
        ]
      , "FROM unvisited_builds"
      , "WHERE provider = ?"
      , "ORDER BY build_num DESC"
      ]

    sql = unlimited_sql <> " LIMIT ?;"


-- | Only searches for CircleCI builds
getUnvisitedBuildsForSha1 ::
     Builds.RawCommit
  -> DbIO [DbHelpers.WithId Builds.UniversalBuild]
getUnvisitedBuildsForSha1 (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ do
    rows <- query conn sql (circleCIProviderIndex, sha1)
    return $ map f rows
  where
    f (universal_build_id, provider_buildnum, provider_id, build_namespace, succeeded, sha1) = DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber provider_buildnum)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit sha1)


    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "universal_build_id"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "commit_sha1"
        ]
      , "FROM unvisited_builds"
      , "WHERE provider = ?"
      , "AND commit_sha1 = ?"
      , "AND NOT succeeded"
      , "ORDER BY universal_build_id DESC"
      ]


-- | TODO Use this
lookupUniversalBuildFromProviderBuild ::
     Connection
  -> Builds.BuildNumber
  -> IO (Maybe (DbHelpers.WithId Builds.UniversalBuild))
lookupUniversalBuildFromProviderBuild conn (Builds.NewBuildNumber build_num) = do
  rows <- query conn sql $ Only build_num
  return $ Safe.headMay rows
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "global_build_num"
        , "build_number"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        , "started_at"
        , "finished_at"
        ]
      , "FROM global_builds WHERE build_number = ?"
      , "ORDER BY provider DESC, global_build_num DESC LIMIT 1;"
      ]


getCachedPullRequestAuthor ::
     Builds.PullRequestNumber
  -> DbIO (Maybe AuthStages.Username)
getCachedPullRequestAuthor (Builds.PullRequestNumber pr_number) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only pr_number
    return $ Safe.headMay $ map (\(Only x) -> AuthStages.Username x) xs
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "github_user_login"
        ]
      , "FROM pull_request_static_metadata"
      , "WHERE pr_number = ?;"
      ]


-- | FIXME partial function
lookupUniversalBuild ::
     Builds.UniversalBuildId -- ^ oldest build number
  -> DbIO (DbHelpers.WithId Builds.UniversalBuild)
lookupUniversalBuild (Builds.UniversalBuildId universal_build_num) = do
  conn <- ask
  liftIO $ head <$> query conn sql (Only universal_build_num)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "id"
        , "build_number"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "commit_sha1"
        ]
      , "FROM universal_builds WHERE id = ?;"
      ]


getUniversalBuilds ::
     Builds.UniversalBuildId -- ^ oldest build number
  -> Int -- ^ limit
  -> DbIO [DbHelpers.WithId Builds.UniversalBuild]
getUniversalBuilds (Builds.UniversalBuildId oldest_universal_build_num) limit = do
  conn <- ask
  liftIO $ query conn sql (oldest_universal_build_num, limit)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "id"
        , "build_number"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "commit_sha1"
        ]
      , "FROM universal_builds"
      , "WHERE id >= ? ORDER BY id ASC LIMIT ?;"
      ]


-- | XXX This is a partial function
getGlobalBuild ::
     Builds.UniversalBuildId
  -> DbIO Builds.StorableBuild
getGlobalBuild (Builds.UniversalBuildId global_build_num) = do
  conn <- ask
  [x] <- liftIO $ query conn sql $ Only global_build_num
  return x
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "global_build_num"
        , "build_number"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        , "started_at"
        , "finished_at"
        ]
      , "FROM global_builds WHERE global_build_num = ?;"
      ]


-- | TODO Get rid of semicolon as delimiter!
-- See: cleanSemicolonDelimitedList, splitAggText
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


data OptOutResponse = OptOutResponse {
    _username           :: AuthStages.Username
  , _disabled           :: Bool
  , _modification_count :: Int
  , _modified_at        :: UTCTime
  } deriving (FromRow, Generic)

instance ToJSON OptOutResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data UserWrapper a = UserWrapper {
    user    :: AuthStages.Username
  , content :: a
  } deriving (FromRow, Generic)

instance (ToJSON a) => ToJSON (UserWrapper a)


canPostPullRequestComments ::
     Connection
  -> AuthStages.Username
  -> IO (Either LT.Text Bool)
canPostPullRequestComments conn (AuthStages.Username author) = do
  [Only is_disabled] <- query conn sql $ Only author
  return $ Right $ not is_disabled
  where
    sql = Q.qjoin [
        "SELECT COALESCE(disabled, FALSE) AS disabled"
      , "FROM (SELECT ? AS myname) foo"
      , "LEFT JOIN pr_comment_posting_opt_outs"
      , "ON foo.myname = pr_comment_posting_opt_outs.username"
      , "LIMIT 1"
      ]


userOptOutSettings :: SqlRead.AuthDbIO (Either Text (UserWrapper (Maybe OptOutResponse)))
userOptOutSettings = do
  SqlRead.AuthConnection conn user@(AuthStages.Username author) <- ask
  xs <- liftIO $ query conn sql $ Only author
  return $ Right $ UserWrapper user $ Safe.headMay xs
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "username"
        , "disabled"
        , "modification_count"
        , "modified_at"
        ]
      , "FROM pr_comment_posting_opt_outs"
      , "WHERE username = ?;"
      ]


getRevisitableWhitelistedBuilds ::
     Connection
  -> [Builds.UniversalBuildId]
  -> IO [(Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
getRevisitableWhitelistedBuilds conn universal_build_ids = do
  putStrLn $ unwords ["Inside", "getRevisitableWhitelistedBuilds"]
  map common_xform <$> query conn sql
    (Only $ In $ map (\(Builds.UniversalBuildId x) -> x) universal_build_ids)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "unscanned_patterns_delimited"
        , "step_id"
        , "step_name"
        , "universal_build"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        ]
      , "FROM unscanned_patterns WHERE universal_build IN ?;"
      ]


getRevisitableBuilds ::
     Connection
  -> IO [(Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild, [Int64])]
getRevisitableBuilds conn =
  map common_xform <$> query_ conn sql
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "unscanned_patterns_delimited"
        , "step_id"
        , "step_name"
        , "universal_build"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        ]
      , "FROM unscanned_patterns;"
      ]


-- | FIXME don't use partial "head"
getLatestPatternId :: Connection -> IO ScanPatterns.PatternId
getLatestPatternId conn =
  head <$> query_ conn sql
  where
    sql = "SELECT id FROM patterns ORDER BY id DESC LIMIT 1;"


apiPostedPRComments ::
     Int
  -> DbIO [PostedComments.PostedComment]
apiPostedPRComments count = do
  conn <- ask
  liftIO $ query conn sql $ Only count
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "pr_number"
        , "sha1"
        , "github_user_login"
        , "body"
        , "created_at"
        , "updated_at"
        , "revision_count"
        ]
      , "FROM latest_created_pull_request_comment_revision"
      , "ORDER BY updated_at DESC"
      , "LIMIT ?;"
      ]


apiPostedStatuses ::
     Int
  -> DbIO [PostedStatuses.PostedStatus]
apiPostedStatuses count = do
  conn <- ask
  liftIO $ query conn sql $ Only count
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "sha1"
        , "description"
        , "state"
        , "created_at"
        ]
      , "FROM created_github_statuses"
      , "ORDER BY created_at DESC"
      , "LIMIT ?;"
      ]


apiPostedStatusesByCommit ::
     Builds.RawCommit
  -> DbIO [PostedStatuses.PostedStatus]
apiPostedStatusesByCommit (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "sha1"
        , "description"
        , "state"
        , "created_at"
        ]
      , "FROM created_github_statuses"
      , "WHERE sha1 = ?"
      , "ORDER BY created_at DESC"
      ]


apiAggregatePostedStatuses :: Int -> DbIO [PostedStatuses.PostedStatusAggregate]
apiAggregatePostedStatuses count = do
  conn <- ask
  liftIO $ query conn sql $ Only count
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "sha1"
        , "count"
        , "last_time"
        , "EXTRACT(SECONDS FROM time_interval)"
        ]
      , "FROM aggregated_github_status_postings LIMIT ?;"
      ]


data WeeklyFailingMergedPullRequests = WeeklyFailingMergedPullRequests {
    _total_pr_count              :: Int
  , _failing_pr_count            :: Int
  , _total_build_count           :: Int
  , _total_failed_build_count    :: Int
  , _foreshadowed_breakage_count :: Int
  } deriving (Generic, FromRow)

instance ToJSON WeeklyFailingMergedPullRequests where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | Note the offset 1 so we only obtain full weeks of data
--
-- Note also the list order reversal for Highcharts
getMergeTimeFailingPullRequestBuildsByWeek ::
     Int
  -> DbIO [DbHelpers.TimestampedDatum WeeklyFailingMergedPullRequests]
getMergeTimeFailingPullRequestBuildsByWeek week_count = do
  conn <- ask
  liftIO $ fmap reverse $ query conn sql $ Only week_count
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "week"
        , "total_pr_count"
        , "failing_pr_count"
        , "total_build_count"
        , "total_failed_build_count"
        , "foreshadowed_breakage_count"
        ]
      , "FROM pr_merge_time_failing_builds_by_week_mview"
      , "WHERE failing_pr_count IS NOT NULL"
      , "ORDER BY week DESC OFFSET 1 LIMIT ?;"
      ]


data PageRequestCounts = PageRequestCounts {
    _page_url :: Text
  , _count    :: Int
  } deriving (Generic, FromRow)

instance ToJSON PageRequestCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | TODO: offset so we only obtain full weeks of data
--
-- Note also the list order reversal for Highcharts
getPageViewsByWeek :: Int -> DbIO [DbHelpers.TimestampedDatum PageRequestCounts]
getPageViewsByWeek _week_count = do
  conn <- ask
--  liftIO $ fmap reverse $ query conn sql $ Only week_count
  liftIO $ fmap reverse $ query_ conn sql
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "week"
        , "url"
        , "request_count"
        ]
      , "FROM frontend_logging.page_requests_by_week"
      , "ORDER BY week DESC"
--      , "OFFSET 1"
--      , "LIMIT ?;"
      ]


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
    timeline_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "pattern_id"
        , "COUNT(*) AS occurrences"
        , "date_trunc('week', queued_at) AS week"
        ]
      , "FROM best_pattern_match_augmented_builds"
      , "WHERE branch IN (SELECT branch FROM presumed_stable_branches)"
      , "GROUP BY pattern_id, week"
      ]


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
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "name"
        , "occurrence_count"
        ]
      , "FROM pattern_build_step_occurrences"
      , "WHERE pattern = ? ORDER BY occurrence_count DESC, name ASC;"
      ]


patternBuildJobOccurrences ::
    ScanPatterns.PatternId
  -> DbIO [WebApi.PieSliceApiRecord]
patternBuildJobOccurrences (ScanPatterns.PatternId patt) = do
  conn <- ask
  liftIO $ query conn sql $ Only patt
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "occurrence_count"
        ]
      , "FROM pattern_build_job_occurrences"
      , "WHERE pattern = ?"
      , "ORDER BY occurrence_count DESC, job_name ASC;"
      ]


apiLineCountHistogram :: DbIO [(Text, Int)]
apiLineCountHistogram = map (swap . f) <$> runQuery sql
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "count(*) AS qty"
        , "pow(10, floor(ln(line_count) / ln(10)))::numeric::integer AS bin"
        ]
      , "FROM log_metadata WHERE line_count > 0"
      , "GROUP BY bin ORDER BY bin ASC;"
      ]


apiByteCountHistogram :: DbIO [(Text, Int)]
apiByteCountHistogram = map (swap . f) <$> runQuery sql
  where
    f = fmap $ \size -> T.pack $ show (size :: Int)
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "COUNT(*) AS qty"
        , "pow(10, floor(ln(byte_count) / ln(10)))::numeric::integer AS bin"
        ]
      , "FROM log_metadata WHERE byte_count > 0"
      , "GROUP BY bin ORDER BY bin ASC;"
      ]


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
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "build_num"
        , "is_flaky"
        , "is_known_broken"
        , "global_build"
        , "provider"
        , "1"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "WHERE sha1 = ? AND NOT succeeded"
      , "ORDER BY job_name;"
      ]


data InclusiveSpan = InclusiveSpan {
    first_value :: Int
  , last_value  :: Int
  } deriving Generic


-- | TODO "job_occurrences" should only include failed builds
apiCommitRangeJobs ::
     InclusiveSpan
  -> DbIO [JobBuild]
apiCommitRangeJobs (InclusiveSpan first_index last_index) = do
  conn <- ask
  liftIO $ query conn sql (first_index, last_index)
  where
    sql = Q.qjoin [
        "SELECT DISTINCT ON (job_name)"
      , Q.list [
          "job_name"
        , "build_num"
        , "is_flaky"
        , "is_known_broken"
        , "global_build"
        , "provider"
        , "count(*) OVER (PARTITION BY job_name) AS job_occurrences"
        ]
      , "FROM (SELECT sha1 FROM ordered_master_commits"
      , "WHERE id >= ? AND id <= ?) foo"
      , "JOIN build_failure_causes ON build_failure_causes.vcs_revision = foo.sha1"
      , "WHERE NOT build_failure_causes.succeeded;"
      ]


getNextMasterCommit ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
getNextMasterCommit conn (Builds.RawCommit current_git_revision) = do
  rows <- query conn sql $ Only current_git_revision

  let mapped_rows = map (\(Only x) -> Builds.RawCommit x) rows
  return $ maybeToEither ("There are no commits that come after " <> current_git_revision) $ Safe.headMay mapped_rows
  where
    sql = Q.qjoin [
        "SELECT sha1 FROM ordered_master_commits"
      , "WHERE id > (SELECT id FROM ordered_master_commits WHERE sha1 = ?)"
      , "ORDER BY id ASC LIMIT 1;"
      ]


apiJobs :: DbIO (WebApi.ApiResponse WebApi.JobApiRecord)
apiJobs = WebApi.ApiResponse . map f <$> runQuery
  "SELECT job_name, freq FROM job_failure_frequencies;"
  where
    f (jobname, freq) = WebApi.JobApiRecord jobname [freq]


apiStep :: DbIO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
apiStep = WebApi.ApiResponse <$> runQuery q
  where
  q = Q.qjoin [
      "SELECT"
    , Q.list [
        "step_name"
      , "COUNT(*) AS freq"
      ]
    , "FROM builds_join_steps"
    , "WHERE step_name IS NOT NULL AND branch IN (SELECT branch FROM presumed_stable_branches)"
    , "GROUP BY step_name ORDER BY freq DESC;"
    ]


apiDeterministicFailureModes :: DbIO (WebApi.ApiResponse WebApi.PieSliceApiRecord)
apiDeterministicFailureModes = WebApi.ApiResponse <$> runQuery q
  where
  q = Q.qjoin [
      "SELECT"
    , Q.list [
        "master_failure_modes.label"
      , "freq"
      ]
    , "FROM (SELECT failure_mode_id, COUNT(*) AS freq FROM known_breakage_summaries GROUP BY failure_mode_id ORDER BY freq DESC) foo"
    , "JOIN master_failure_modes"
    , "ON foo.failure_mode_id = master_failure_modes.id;"
    ]


data DownstreamCommitInfo = DownstreamCommitInfo {
    _sha1     :: Builds.RawCommit
  , _distance :: Int
  } deriving (Generic, FromRow)

instance ToJSON DownstreamCommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiMasterDownstreamCommits :: Builds.RawCommit -> DbIO [DownstreamCommitInfo]
apiMasterDownstreamCommits (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
  sql = Q.qjoin [
      "SELECT branch_commit, distance"
    , "FROM pr_merge_bases"
    , "WHERE master_commit = ?"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
-- thus, use of reverse
apiStatusNotificationsByHour :: Int -> DbIO (WebApi.ApiResponse (UTCTime, Int))
apiStatusNotificationsByHour hours = do
  conn <- ask
  liftIO $ WebApi.ApiResponse . reverse <$> query conn sql (Only hours)
  where
  sql = Q.qjoin [
      "SELECT"
    , "date_trunc('hour', created_at) AS hour, COUNT(*)"
    , "FROM github_incoming_status_events"
    , "GROUP BY hour ORDER BY hour DESC"
    , "OFFSET 1"
    , "LIMIT ?"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
apiIsolatedMasterFailuresByDay :: Int -> DbIO (Either Text [(Day, Double)])
apiIsolatedMasterFailuresByDay day_count = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only day_count
    return $ Right $ reverse xs
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "date_california_time"
      , "isolated_failure_fraction"
      ]
      -- This view already exludes the current (incomplete) day
    , "FROM master_daily_isolated_failures_cached"
    , "WHERE date_california_time > (now() - interval '? days')"
    , "ORDER BY date_california_time DESC"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
apiFailedCommitsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiFailedCommitsByDay = WebApi.ApiResponse <$> runQuery q
  where
  q = Q.qjoin [
      "SELECT"
    , "queued_at::date AS date, COUNT(*)"
    , "FROM (SELECT vcs_revision, MAX(queued_at) queued_at FROM global_builds GROUP BY vcs_revision) foo"
    , "GROUP BY date ORDER BY date ASC;"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
apiStatusPostedCommitsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiStatusPostedCommitsByDay = WebApi.ApiResponse . reverse <$> runQuery q
  where
  q = Q.qjoin [
      "SELECT"
    , "last_time::date AS date, COUNT(*)"
    , "FROM aggregated_github_status_postings"
    , "GROUP BY date ORDER BY date DESC OFFSET 1;"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
apiStatusPostingsByDay :: DbIO (WebApi.ApiResponse (Day, Int))
apiStatusPostingsByDay = WebApi.ApiResponse . reverse <$> runQuery q
  where
  q = Q.qjoin [
      "SELECT"
    , "created_at::date AS date, COUNT(*)"
    , "FROM created_github_statuses"
    , "GROUP BY date ORDER BY date DESC OFFSET 1;"
    ]


listBuilds :: Query -> DbIO [WebApi.BuildBranchRecord]
listBuilds sql = do
  conn <- ask
  liftIO $ query_ conn sql


apiUnmatchedBuilds :: DbIO [WebApi.BuildBranchDateRecord]
apiUnmatchedBuilds = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "global_build"
    , "step_name"
    , "job_name"
    , "vcs_revision"
    , "queued_at"
    ]
  , "FROM unattributed_failed_builds"
  , "ORDER BY queued_at DESC"
  , "LIMIT 1000"
  ]


apiIdiopathicBuilds :: DbIO [WebApi.BuildBranchRecord]
apiIdiopathicBuilds = listBuilds $ Q.qjoin [
    "SELECT"
  , "branch, global_build_num"
  , "FROM idiopathic_build_failures"
  , "ORDER BY global_build_num DESC;"
  ]


apiUnmatchedCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [WebApi.UnmatchedBuild])
apiUnmatchedCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ PostgresHelpers.catchDatabaseError catcher $ do
    xs <- query conn sql $ Only sha1
    return $ Right xs
  where

    catcher _ (PostgresHelpers.QueryCancelled some_error) = return $ Left $ "Query error in apiUnmatchedCommitBuilds: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "unattributed_failed_builds.build_number"
        , "unattributed_failed_builds.step_name"
        , "unattributed_failed_builds.queued_at"
        , "unattributed_failed_builds.job_name"
        , "unattributed_failed_builds.branch"
        , "unattributed_failed_builds.global_build"
        , "ci_providers.icon_url"
        , "ci_providers.label"
        ]
      , "FROM unattributed_failed_builds"
      , "JOIN ci_providers"
      , "ON unattributed_failed_builds.provider = ci_providers.id"
      , "WHERE unattributed_failed_builds.vcs_revision = ?;"
      ]


apiIdiopathicCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [WebApi.UnmatchedBuild])
apiIdiopathicCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ Right . map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, universal_build_id, provider_icon_url, provider_label) =
      WebApi.UnmatchedBuild
        (Builds.NewBuildNumber build)
        step_name
        queued_at
        job_name
        branch
        (Builds.UniversalBuildId universal_build_id)
        provider_icon_url
        provider_label

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "build_num"
        , "step_name"
        , "queued_at"
        , "job_name"
        , "idiopathic_build_failures.branch"
        , "builds_join_steps.universal_build"
        , "ci_providers.icon_url"
        , "ci_providers.label"
        ]
      , "FROM idiopathic_build_failures"
      , "JOIN builds_join_steps"
      , "ON idiopathic_build_failures.global_build_num = builds_join_steps.universal_build"
      , "JOIN ci_providers"
      , "ON builds_join_steps.provider = ci_providers.id"
      , "WHERE vcs_revision = ?;"
      ]


apiTimeoutCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [WebApi.UnmatchedBuild])
apiTimeoutCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ fmap Right $ query conn sql $ Only sha1
  where
    sql = Q.qjoin [
        "SELECT"
      , "build_num, step_name, queued_at, job_name, branch, universal_build, ci_providers.icon_url, ci_providers.label"
      , "FROM builds_join_steps"
      , "JOIN ci_providers"
      , "ON builds_join_steps.provider = ci_providers.id"
      , "WHERE vcs_revision = ? AND is_timeout;"
      ]


-- | Obtains subset of console log from database
readLogSubset ::
     MatchOccurrences.MatchId
  -> Int -- ^ offset
  -> Int -- ^ limit
  -> DbIO [LT.Text]
readLogSubset (MatchOccurrences.MatchId match_id) offset limit = do
  conn <- ask
  xs <- liftIO $ query conn sql (match_id, offset, limit)
  return $ map (\(Only log_text) -> log_text) xs
  where
    sql = Q.qjoin [
        "SELECT UNNEST(COALESCE(content_lines, regexp_split_to_array(content, '\n')))"
      , "FROM log_metadata"
      , "JOIN matches"
      , "ON matches.build_step = log_metadata.step"
      , "WHERE matches.id = ?"
      , "OFFSET ? LIMIT ?;"
      ]


-- | Obtains the console log from database
readLog ::
     Builds.BuildStepId
  -> DbIO (Maybe LT.Text)
readLog (Builds.NewBuildStepId step_id) = do
  conn <- ask
  result <- liftIO $ query conn sql $ Only step_id
  return $ (\(Only log_text) -> log_text) <$> Safe.headMay result
  where
    sql = Q.qjoin [
        "SELECT COALESCE(array_to_string(content_lines, e'\n'), content)"
      , "FROM log_metadata"
      , "WHERE step = ? LIMIT 1;"
      ]


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
masterBuildFailureStats = fmap head $ runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "count(*) AS total"
    , "sum(is_idiopathic::int) AS idiopathic"
    , "sum(is_timeout::int) AS timeout"
    , "sum(is_known_broken::int) AS known_broken"
    , "sum((NOT is_unmatched)::int) AS pattern_matched"
    , "sum(is_flaky::int) AS flaky"
    ]
  , "FROM build_failure_causes"
  , "JOIN ordered_master_commits"
  , "ON build_failure_causes.vcs_revision = ordered_master_commits.sha1;"
  ]


-- | Uses OFFSET 1 so we only ever show full weeks
masterWeeklyFailureStats ::
     Int
  -> DbIO (Either Text WeeklyStats.MasterStatsBundle)
masterWeeklyFailureStats week_count = do

  conn <- ask
  xs <- liftIO $ query conn sql $ Only week_count
  return $ do
    checked_rows <- mapM f xs
    return $ WeeklyStats.MasterStatsBundle
      WeeklyStats.buildCountColors
      (reverse checked_rows)

  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "commit_count"
        , "had_failure"
        , "had_idiopathic"
        , "had_timeout"
        , "had_known_broken"
        , "had_pattern_matched"
        , "had_flaky"
        , "failure_count::int"
        , "idiopathic_count::int"
        , "timeout_count::int"
        , "known_broken_count::int"
        , "pattern_matched_count::int"
        , "pattern_unmatched_count::int"
        , "flaky_count::int"
        , "earliest_commit_index"
        , "latest_commit_index"
        , "week"
        , "sanity_check_is_failure_count_equal"
        , "sanity_check_total_is_successes_plus_failures"
        ]
      , "FROM master_failures_weekly_aggregation_mview"
      , "ORDER BY week DESC LIMIT ? OFFSET 1;"
      ]

    f (
        commit_count
      , had_failure
      , had_idiopathic
      , had_timeout
      , had_known_broken
      , had_pattern_matched
      , had_flaky
      , failure_count
      , idiopathic_count
      , timeout_count
      , known_broken_count
      , pattern_matched_count
      , pattern_unmatched_count
      , flaky_count
      , earliest_commit_index
      , latest_commit_index
      , week
      , sanity_check_is_failure_count_equal
      , sanity_check_total_is_successes_plus_failures
      ) = do

      unless sanity_check_is_failure_count_equal $
        Left $ "Sanity check failed: sanity_check_is_failure_count_equal"

      unless sanity_check_total_is_successes_plus_failures $
        Left $ "Sanity check failed: sanity_check_total_is_successes_plus_failures"

      return $ WeeklyStats.MasterWeeklyStats
        commit_count
        agg_commit_counts
        agg_build_counts
        week $ DbHelpers.InclusiveNumericBounds
          earliest_commit_index
          latest_commit_index

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


data MonthlyBreakageStats = MonthlyBreakageStats {
    _distinct_breakages :: Int
  , _avoidable_count    :: Int
  } deriving (Generic, FromRow)

instance ToJSON MonthlyBreakageStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | TODO Should use OFFSET 1 so we only ever show full months.
-- However, we don't have enough data yet.
--
-- Note also the reversal for Highcharts
masterBreakageMonthlyStats :: DbIO [DbHelpers.TimestampedDatum MonthlyBreakageStats]
masterBreakageMonthlyStats = do

  conn <- ask
  xs <- liftIO $ query_ conn sql
  return $ reverse xs
  where
    sql = Q.qjoin [
        "SELECT"
      , "month, distinct_breakages, avoidable_count"
      , "FROM code_breakage_monthly_aggregation"
      , "ORDER BY month DESC;"
      ]


-- | Uses OFFSET 1 so we only ever show full weeks
downstreamWeeklyFailureStats :: Int -> DbIO [BuildResults.WeeklyBreakageImpactStats]
downstreamWeeklyFailureStats week_count = do

  conn <- ask
  xs <- liftIO $ query conn sql $ Only week_count
  return $ reverse $ map f xs
  where
    f (week, distinct_breakages, downstream_broken_commit_count, downstream_broken_build_count, unavoidable_downstream_broken_commit_count, unavoidable_downstream_broken_build_count) = BuildResults.WeeklyBreakageImpactStats
      week
      distinct_breakages
      total_impact
      unavoidable_impact
      where
        total_impact = BuildResults.DownstreamImpactCounts
          downstream_broken_commit_count
          downstream_broken_build_count

        unavoidable_impact = BuildResults.DownstreamImpactCounts
          unavoidable_downstream_broken_commit_count
          unavoidable_downstream_broken_build_count

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "week"
        , "distinct_breakages"
        , "downstream_broken_commit_count"
        , "downstream_broken_build_count"
        , "unavoidable_downstream_broken_commit_count"
        , "unavoidable_downstream_broken_build_count"
        ]
      , "FROM upstream_breakages_weekly_aggregation_mview"
      , "ORDER BY week DESC"
      , "LIMIT ? OFFSET 1;"
      ]


getLatestKnownMasterCommit :: Connection -> IO (Maybe Text)
getLatestKnownMasterCommit conn = do
  rows <- query_ conn sql
  return $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = Q.qjoin [
        "SELECT sha1 FROM ordered_master_commits"
      , "ORDER BY id DESC LIMIT 1;"
      ]


isMasterCommit :: Builds.RawCommit -> DbIO Bool
isMasterCommit (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ do
    [Only exists] <- query conn master_commit_retrieval_sql $ Only sha1
    return exists
  where
    master_commit_retrieval_sql = "SELECT EXISTS (SELECT * FROM ordered_master_commits WHERE sha1 = ?);"


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
  } deriving (Show, Generic)

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
    sql = Q.qjoin [
        "SELECT"
      , "reporter, reported_at, job"
      , "FROM code_breakage_affected_jobs"
      , "WHERE cause = ? ORDER BY job ASC;"
      ]


data UpstreamBrokenJob = UpstreamBrokenJob {
    _job_name            :: Text
  , _breakage_start_time :: UTCTime
  , _breakage_end_time   :: Maybe UTCTime
  , _breakage_start_sha1 :: Builds.RawCommit
  , _breakage_end_sha1   :: Maybe Builds.RawCommit
  } deriving (Generic, FromRow)

instance ToJSON UpstreamBrokenJob where
  toJSON = genericToJSON JsonUtils.dropUnderscore


extractJobName :: UpstreamBrokenJob -> Text
extractJobName (UpstreamBrokenJob x _ _ _ _) = x


-- | Compare to: getSpanningBreakages
--
-- This query is only valid after the PR commit ancestor in the master branch
-- is cached in the database.
--
-- It does not need to know the master commit (merge base).
getInferredSpanningBrokenJobsBetter ::
     Builds.RawCommit -- ^ branch commit
  -> DbIO [UpstreamBrokenJob]
getInferredSpanningBrokenJobsBetter (Builds.RawCommit branch_sha1) = do
  conn <- ask
  liftIO $ query conn sql $ Only branch_sha1
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "open_date"
        , "closed_date"
        , "open_sha1"
        , "closed_sha1"
        ]
      , "FROM downstream_build_failures_from_upstream_inferred_breakages"
      , "WHERE branch_commit = ?"
      ]


-- | This only works for commits from the master branch.
-- Commits from other branches must use
-- StatusUpdate.findKnownBuildBreakages
--
-- | Compare to: getInferredSpanningBrokenJobs
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

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "code_breakage_cause.sha1"
        , "code_breakage_cause.description"
        , "cause_id"
        , "COALESCE(jobs, ''::text) AS jobs"
        ]
      , "FROM"
      , Q.parens inner_q
      , "foo"
      , "JOIN code_breakage_cause"
      , "ON foo.cause_id = code_breakage_cause.id;"
      ]

    inner_q = Q.qjoin [
        "SELECT"
      , Q.list [
          "code_breakage_spans.cause_id"
        , "string_agg((code_breakage_affected_jobs.job)::text, ';'::text) AS jobs"
        ]
      , "FROM code_breakage_spans"
      , "LEFT JOIN code_breakage_affected_jobs"
      , "ON code_breakage_affected_jobs.cause = code_breakage_spans.cause_id"
      , "WHERE cause_commit_index <= ? AND (resolved_commit_index IS NULL OR ? < resolved_commit_index)"
      , "GROUP BY code_breakage_spans.cause_id"
      ]


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
apiTagsHistogram = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "tag"
    , "COUNT(*) AS pattern_count"
    , "SUM(matching_build_count)::bigint AS build_matches"
    ]
  , "FROM pattern_tags"
  , "LEFT JOIN pattern_frequency_summary_partially_cached"
  , "ON pattern_frequency_summary_partially_cached.id = pattern_tags.pattern"
  , "GROUP BY tag"
  , "ORDER BY pattern_count DESC, build_matches DESC;"
  ]


data MasterCommitAndSourcePr = MasterCommitAndSourcePr {
    _sha1      :: Builds.RawCommit
  , _pr_number :: Builds.PullRequestNumber
  } deriving (Generic, FromRow)

instance ToJSON MasterCommitAndSourcePr where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getAllMergedPullRequestHeadCommits :: DbIO [Builds.RawCommit]
getAllMergedPullRequestHeadCommits = runQuery $ Q.qjoin [
    "SELECT pr_head_commit"
  , "FROM pr_merge_time_build_stats_by_master_commit"
  , "ORDER BY commit_number DESC;"
  ]


getPullRequestsWithMissingHeads :: DbIO [Builds.PullRequestNumber]
getPullRequestsWithMissingHeads = do
  xs <- runQuery sql
  return $ map (\(Only x) -> Builds.PullRequestNumber x) xs
  where
    sql = Q.qjoin [
        "SELECT"
      , "mv.github_pr_number"
      , "FROM master_ordered_commits_with_metadata_mview mv"
      , "LEFT JOIN pull_request_heads"
      , "ON pull_request_heads.pr_number = mv.github_pr_number"
      , "WHERE"
      , Q.qconjunction [
          "github_pr_number IS NOT NULL"
        , "pull_request_heads.pr_number IS NULL"
        ]
      , "ORDER BY mv.id DESC"
      ]


getAllMasterCommitPullRequests :: DbIO [MasterCommitAndSourcePr]
getAllMasterCommitPullRequests = runQuery $ Q.qjoin [
    "SELECT"
  , "sha1, github_pr_number"
  , "FROM master_ordered_commits_with_metadata"
  , "WHERE github_pr_number IS NOT NULL"
  , "ORDER BY id DESC;"
  ]


-- | Gets Pull Request numbers of the commits that have been
-- implicated in master branch breakages
getImplicatedMasterCommitPullRequests :: DbIO [MasterCommitAndSourcePr]
getImplicatedMasterCommitPullRequests = runQuery $ Q.qjoin [
    "SELECT"
  , "cause_sha1, github_pr_number"
  , "FROM known_breakage_summaries_sans_impact"
  , "WHERE github_pr_number IS NOT NULL"
  , "ORDER BY cause_commit_index DESC;"
  ]


apiAutocompleteTags :: Text -> DbIO [Text]
apiAutocompleteTags = listFlat1X $ Q.qjoin [
    "SELECT tag FROM"
  , "(SELECT tag, COUNT(*) AS freq"
  , "FROM pattern_tags"
  , "GROUP BY tag ORDER BY freq DESC, tag ASC) foo"
  , "WHERE tag ILIKE CONCAT(?,'%');"
  ]


apiAutocompleteSteps :: Text -> DbIO [Text]
apiAutocompleteSteps = listFlat1X $ Q.qjoin [
    "SELECT name FROM"
  , "(SELECT name, COUNT(*) AS freq FROM"
  , "build_steps_deduped_mitigation WHERE name IS NOT NULL"
  , "GROUP BY name ORDER BY freq DESC, name ASC) foo"
  , "WHERE name ILIKE CONCAT(?,'%');"
  ]


apiListSteps :: DbIO [Text]
apiListSteps = listFlat $ Q.qjoin [
    "SELECT name FROM build_steps_deduped_mitigation"
  , "WHERE name IS NOT NULL"
  , "GROUP BY name"
  , "ORDER BY COUNT(*) DESC, name ASC;"
  ]


-- | Excludes pattern match aggregate counts since they (for now)
-- are more expensive to compute
data BasicRevisionBuildStats = BasicRevisionBuildStats {
    _total        :: Int
  , _idiopathic   :: Int
  , _timeout      :: Int
  , _known_broken :: Int
  , _succeeded    :: Int
  , _failed       :: Int
  } deriving (Generic, FromRow)


-- | This is almost redundant with the existing "build_failure_disjoint_causes_by_commit"
-- view but is re-implemented here to allow filtering by provider
getNonPatternMatchRevisionStats ::
     Builds.RawCommit
  -> DbIO (Either LT.Text BasicRevisionBuildStats)
getNonPatternMatchRevisionStats (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ maybeToEither err . Safe.headMay <$> query conn sql (sha1, SqlRead.circleCIProviderIndex)
  where
    err = LT.unwords [
        "No match for commit"
      , LT.fromStrict sha1
      ]

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "count(build_failure_causes_disjoint.vcs_revision) AS total"
        , "COALESCE(sum(build_failure_causes_disjoint.is_idiopathic::integer), 0::bigint) AS idiopathic"
        , "COALESCE(sum(build_failure_causes_disjoint.is_timeout::integer), 0::bigint) AS timeout"
        , "COALESCE(sum(build_failure_causes_disjoint.is_known_broken::integer), 0::bigint) AS known_broken"
        , "COALESCE(sum(build_failure_causes_disjoint.succeeded::integer), 0::bigint) AS succeeded"
        , "count(build_failure_causes_disjoint.vcs_revision) - COALESCE(sum(build_failure_causes_disjoint.succeeded::integer), 0::bigint) AS failed"
        ]
      , "FROM build_failure_causes_disjoint"
      , "WHERE build_failure_causes_disjoint.vcs_revision = ?"
      , "AND provider = ?"
      , "GROUP BY build_failure_causes_disjoint.vcs_revision"
      , "LIMIT 1"
      ]


getBuildsParameterized :: ToRow q =>
     q
  -> [Query]
  -> DbIO (DbHelpers.BenchmarkedResponse Float [CommitBuilds.CommitBuild])
getBuildsParameterized sql_parms sql_where_conditions = do
  conn <- ask

  (timing, content) <- MyUtils.timeThisFloat $ liftIO $ query conn sql sql_parms
  return $ DbHelpers.BenchmarkedResponse timing content
  where

    -- TODO FIXME
    -- This is copying the logic from multiple nested views so that
    -- a query for a single git revision is optimized.
    -- Beware especially of divergence of the match ranking logic (e.g. on "specificity"),
    -- if the logic is updated in the VIEW definition on the database side.
    --
    -- See Github Issue #52
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "build_steps.name AS step_name"
        , "match_id"
        , "build"
        , "global_builds.vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        , "pattern_id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "specificity"
        , "global_builds.global_build_num AS universal_build"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "label"
        , "icon_url"
        , "started_at"
        , "finished_at"
        , "FALSE as is_timeout"
        ]
      , "FROM"
      , Q.parens best_match_subquery_sql
      , "best_pattern_match_for_builds"
      , "JOIN matches ON matches.id = best_pattern_match_for_builds.match_id"
      , "JOIN log_metadata ON log_metadata.step = best_pattern_match_for_builds.step_id"
      , "JOIN global_builds ON global_builds.global_build_num = best_pattern_match_for_builds.universal_build"
      , "JOIN build_steps ON build_steps.universal_build = best_pattern_match_for_builds.universal_build"
      , "JOIN ci_providers"
      , "ON ci_providers.id = global_builds.provider"
      ]


    best_match_subquery_sql = Q.qjoin [
        "SELECT DISTINCT ON (matches_for_build.universal_build)"
      , Q.list [
          "matches_for_build.build"
        , "matches_for_build.pat AS pattern_id"
        , "patterns_rich.expression"
        , "patterns_rich.regex"
        , "patterns_rich.is_retired"
        , "patterns_rich.specificity"
        , "matches_for_build.universal_build"
        , "matches_for_build.match_id"
        , "matches_for_build.step_id"
        , "matches_for_build.vcs_revision"
        ]
      , "FROM matches_for_build"
      , "JOIN patterns_rich ON matches_for_build.pat = patterns_rich.id"
      , "WHERE"
      , Q.qconjunction sql_where_conditions
        -- BEWARE OF DIVERGENCE OF THIS LOGIC!
      , "ORDER BY"
      , Q.list [
          "matches_for_build.universal_build"
        , "patterns_rich.specificity DESC"
        , "patterns_rich.is_retired"
        , "patterns_rich.regex"
        , "patterns_rich.id DESC"
        , "matches_for_build.match_id DESC"
        ]
      ]


    -- THIS is not used for now; it is perserved so that at some point
    -- we can go back to this simpler query.
    _sql_unoptimized = Q.qjoin [
        "SELECT"
      , Q.list [
          "step_name"
        , "match_id"
        , "build"
        , "vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        , "pattern_id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "specificity"
        , "universal_build"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "label"
        , "icon_url"
        , "started_at"
        , "finished_at"
        ]
      , "FROM best_pattern_match_augmented_builds"
      , "JOIN ci_providers"
      , "ON ci_providers.id = best_pattern_match_augmented_builds.provider"
      , "WHERE vcs_revision = ?;"
      ]


-- | For commit-details page
getRevisionBuilds ::
     GitRev.GitSha1
  -> DbIO (Either Text (DbHelpers.BenchmarkedResponse Float [CommitBuilds.CommitBuild]))
getRevisionBuilds git_revision = do
  conn <- ask
  liftIO $ PostgresHelpers.catchDatabaseError catcher $ do
    x <- runReaderT (getBuildsParameterized sql_parms sql_where_conditions) conn
    return $ Right x

  where

    catcher _ (PostgresHelpers.QueryCancelled some_error) = return $ Left $ "Query error in getRevisionBuilds: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


    sql_parms = Only $ GitRev.sha1 git_revision
    sql_where_conditions = [
        "vcs_revision = ?"
      ]


apiGetMasterCommits ::
     Pagination.ParentOffsetMode
  -> DbIO (Either Text (DbHelpers.InclusiveNumericBounds Int64, [BuildResults.IndexedRichCommit]))
apiGetMasterCommits parent_offset_mode = do
  conn <- ask
  liftIO $ getMasterCommits conn parent_offset_mode


getMasterCommits ::
     Connection
  -> Pagination.ParentOffsetMode
  -> IO (Either Text (DbHelpers.InclusiveNumericBounds Int64, [BuildResults.IndexedRichCommit]))
getMasterCommits conn parent_offset_mode =

  case parent_offset_mode of
    Pagination.CommitIndices bounds@(DbHelpers.InclusiveNumericBounds minbound maxbound) -> do

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

      return (DbHelpers.InclusiveNumericBounds first_commit_index latest_id, mapped_rows)

  where
    f ( commit_id
      , commit_sha1
      , commit_number
      , maybe_pr_number
      , maybe_message
      , maybe_tree_sha1
      , maybe_author_name
      , maybe_author_email
      , maybe_author_date
      , maybe_committer_name
      , maybe_committer_email
      , maybe_committer_date
      , was_built
      , populated_config_yaml
      , downstream_commit_count
      , reverted_sha1
      , total_required_commit_job_count
      , failed_or_incomplete_required_job_count
      , failed_required_job_count
      , disqualifying_jobs_array) =
      DbHelpers.WithId commit_id $ BuildResults.CommitAndMetadata
        wrapped_sha1
        maybe_metadata
        commit_number
        maybe_pr_number
        was_built
        populated_config_yaml
        downstream_commit_count
        reverted_sha1
        maybe_required_job_counts

      where
        maybe_required_job_counts = BuildResults.RequiredJobCounts <$>
          total_required_commit_job_count <*>
          failed_or_incomplete_required_job_count <*>
          failed_required_job_count <*>
          disqualifying_jobs_array

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

    sql_first_commit_id = "SELECT id FROM ordered_master_commits ORDER BY id DESC LIMIT 1 OFFSET ?;"
    sql_associated_commit_id = "SELECT id FROM ordered_master_commits WHERE sha1 = ?;"

    commits_query_prefix = Q.qjoin [
        "SELECT"
      , Q.list [
            "id"
          , "sha1"
          , "commit_number"
          , "github_pr_number"
          , "message"
          , "tree_sha1"
          , "author_name"
          , "author_email"
          , "author_date"
          , "committer_name"
          , "committer_email"
          , "committer_date"
          , "was_built"
          , "populated_config_yaml"
          , "downstream_commit_count"
          , "reverted_sha1"
          , "total_required_commit_job_count"
          , "not_succeeded_required_job_count"
          , "failed_required_job_count"
          , "disqualifying_jobs_array"
          ]
      , "FROM master_ordered_commits_with_metadata_mview"
      ]

    sql_commit_id_and_offset = Q.qjoin [
        commits_query_prefix
      , "WHERE id <= ?"
      , "ORDER BY id DESC"
      , "LIMIT ?;"
      ]

    sql_commit_id_bounds = Q.qjoin [
        commits_query_prefix
      , "WHERE id >= ? AND id <= ?"
      , "ORDER BY id DESC;"
      ]


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
  fromRow =
    CommitBreakageRegionCounts <$> fromRow <*> field <*> field <*> field


data MissingJobStats = MissingJobStats {
    _job_name          :: Text
  , _count             :: Int
  , _latest_absence_at :: UTCTime
  } deriving (Generic, FromRow)

instance ToJSON MissingJobStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore



data ScanQueueEntry = ScanQueueEntry {
    _sha1        :: Builds.RawCommit
  , _inserted_at :: UTCTime
  } deriving (Generic, FromRow)

instance ToJSON ScanQueueEntry where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiScanCommitsQueue :: DbIO [ScanQueueEntry]
apiScanCommitsQueue = runQuery sql
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "sha1"
      , "inserted_at"
      ]
    , "FROM work_queues.queued_sha1_scans"
    , "ORDER BY inserted_at DESC"
    ]


apiMissingRequiredBuilds :: DbIO [MissingJobStats]
apiMissingRequiredBuilds = runQuery sql
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "job_name"
      , "COUNT(*) AS count"
      , "MAX(master_commits_basic_metadata.committer_date) AS latest_absence_at"
--      , "MAX(master_required_unbuilt_jobs_mview.id) AS latest_id"
--      , "array_agg(master_required_unbuilt_jobs_mview.id) AS commit_ids"
      ]
    , "FROM master_required_unbuilt_jobs_mview"
    , "JOIN master_commits_basic_metadata ON"
    , "master_commits_basic_metadata.id = master_required_unbuilt_jobs_mview.id"
    , "WHERE tstzrange(now() - interval '7 days', now()) @> master_commits_basic_metadata.committer_date"
    , "GROUP BY master_required_unbuilt_jobs_mview.job_name"
    , "ORDER BY count DESC"
    ]


apiLeftoverCodeBreakagesByCommit :: DbIO [CommitBreakageRegionCounts]
apiLeftoverCodeBreakagesByCommit = runQuery $ Q.qjoin [
    "SELECT id, vcs_revision, only_longitudinal_breakages, only_lateral_breakages, both_breakages"
  , "FROM master_unmarked_breakage_regions_by_commit;"
  ]


apiLeftoverDetectedCodeBreakages :: DbIO [NonannotatedBuildBreakages]
apiLeftoverDetectedCodeBreakages = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "master_detected_breakages_without_annotations.contiguous_group_position"
    , "master_detected_breakages_without_annotations.contiguous_group_index"
    , "master_detected_breakages_without_annotations.contiguous_start_commit_index"
    , "master_detected_breakages_without_annotations.contiguous_end_commit_index"
    , "master_detected_breakages_without_annotations.contiguous_length"
    , "master_detected_breakages_without_annotations.cluster_id"
    , "master_detected_breakages_without_annotations.cluster_member_count"
    , "builds_join_steps.universal_build"
    , "builds_join_steps.build_num"
    , "builds_join_steps.provider"
    , "builds_join_steps.build_namespace"
    , "builds_join_steps.succeeded"
    , "builds_join_steps.vcs_revision"
    ]
  , "FROM master_detected_breakages_without_annotations"
  , "JOIN builds_join_steps"
  , "ON master_detected_breakages_without_annotations.universal_build = builds_join_steps.universal_build;"
  ]


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


-- | TODO: Use runExceptT here
refreshCachedMasterGrid ::
     Text
  -> Bool -- ^ triggered from frontend
  -> DbIO (Either Text ())
refreshCachedMasterGrid view_name is_from_frontend = do
  conn <- ask
  liftIO $ case either_query of
    Left x -> return $ Left x
    Right sql_query -> do

      MyUtils.debugList [
          "Refreshing view"
        , MyUtils.quote $ T.unpack view_name
        ]

      (execution_time, _) <- MyUtils.timeThisFloat $ execute_ conn sql_query

      execute conn "INSERT INTO lambda_logging.materialized_view_refresh_events (view_name, execution_duration_seconds, event_source) VALUES (?, ?, ?);" (view_name, execution_time, trigger_source)

      MyUtils.debugStr "View refreshed."
      return $ Right ()

  where
    trigger_source :: Text
    trigger_source = if is_from_frontend
      then "frontend"
      else "lambda"

    -- TODO Would be nice not to have to hard-code these queries...
    either_query = case view_name of
      "master_failures_raw_causes_mview" -> Right "REFRESH MATERIALIZED VIEW CONCURRENTLY master_failures_raw_causes_mview;"
      "upstream_breakages_weekly_aggregation_mview" -> Right "REFRESH MATERIALIZED VIEW CONCURRENTLY upstream_breakages_weekly_aggregation_mview;"
      _ -> Left $ "Unrecognized vew name: " <> view_name


getLastCachedMasterGridRefreshTime :: DbIO (UTCTime, Text)
getLastCachedMasterGridRefreshTime = do
  conn <- ask
  liftIO $ do
    [tuple] <- query conn sql (Only ("master_failures_raw_causes_mview" :: String))
    return tuple
  where
    sql = Q.qjoin [
        "SELECT timestamp, event_source"
      , "FROM lambda_logging.materialized_view_refresh_events"
      , "WHERE view_name = ?"
      , "ORDER BY timestamp DESC LIMIT 1;"
      ]


-- | Get the most recent queued_at time of a build that was
-- fetched via the provider-specific API.
getMostRecentProviderApiFetchedBuild ::
     Int64 -- ^ provider ID
  -> Text -- ^ branch name
  -> DbIO (Maybe UTCTime)
getMostRecentProviderApiFetchedBuild provider_id branch_name = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql (provider_id, branch_name)
    return $ Safe.headMay $ map (\(Only x) -> x) xs
  where
    sql = Q.qjoin [
        "SELECT latest_queued_at"
      , "FROM ci_provider_scan_ranges"
      , "WHERE"
      , Q.qconjunction [
          "provider = ?"
        , "branch_filter = ?"
        ]
      , "ORDER BY latest_queued_at DESC LIMIT 1;"
      ]


data PostedPRComment = PostedPRComment {
    _pr_number      :: Int
  , _comment_id     :: Int64
  , _revision_id    :: Int64
  , _body           :: Text
  , _created_at     :: UTCTime
  , _updated_at     :: UTCTime
  , _revision_count :: Int
  } deriving (Generic, FromRow)


getPostedCommentForPR ::
     Builds.PullRequestNumber
  -> DbIO (Maybe PostedPRComment)
getPostedCommentForPR (Builds.PullRequestNumber pr_number) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only pr_number
    return $ Safe.headMay xs
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "pr_number"
        , "comment_id"
        , "id"
        , "body"
        , "created_at"
        , "updated_at"
        , "revision_count"
        ]
      , "FROM latest_created_pull_request_comment_revision"
      , "WHERE pr_number = ?"
      , "LIMIT 1;"
      ]


data MaterializedViewRefreshInfo = MaterializedViewRefreshInfo {
    _view_name              :: Text
  , _latest                 :: UTCTime
  , _average_execution_time :: Double
  , _event_count            :: Int
  , _latest_age_seconds     :: Double
  } deriving (Generic, FromRow)

instance ToJSON MaterializedViewRefreshInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiMaterializedViewRefreshes :: DbIO [MaterializedViewRefreshInfo]
apiMaterializedViewRefreshes = runQuery sql
  where
    sql = Q.qjoin [
          "SELECT"
        , Q.list [
            "view_name"
          , "latest"
          , "average_execution_time"
          , "event_count"
          , "EXTRACT(EPOCH FROM latest_age) AS latest_age_seconds"
          ]
      , "FROM lambda_logging.materialized_view_refresh_event_stats"
      , "ORDER BY latest_age;"
      ]


-- | TODO: Not only are we hard-coding the variance threshold,
-- but we also need to hardcode the prefix of "binary_"
-- since the variance threshold is not completely reliable.
--
-- FIXME This is legacy. Don't use this!
getScheduledJobNames :: DbIO [Text]
getScheduledJobNames = listFlat sql
  where
    sql = Q.qjoin [
        "SELECT"
      , "job_name FROM job_schedule_discriminated_mview"
      , "WHERE inferred_scheduled"
      , "ORDER BY job_name;"
      ]


data MasterJobCoverage = MasterJobCoverage {
    _commit_id                        :: Int64
  , _sha1                             :: Builds.RawCommit
  , _total_required_commit_job_count  :: Int
  , _not_succeeded_required_job_count :: Int
  , _failed_required_job_count        :: Int
  , _disqualifying_jobs               :: DbHelpers.SemicolonDelimitedDbText
  , _commit_timestamp                 :: Maybe UTCTime
  , _age_hours                        :: Maybe Double
  } deriving (FromRow, Generic)

instance ToJSON MasterJobCoverage where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiCleanestMasterCommits ::
     Int
  -> Int
  -> DbIO [MasterJobCoverage]
apiCleanestMasterCommits missing_threshold failing_threshold = do
  conn <- ask
  liftIO $ query conn sql (missing_threshold, failing_threshold)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "commit_id"
        , "sha1"
        , "total_required_commit_job_count"
        , "not_succeeded_required_job_count"
        , "failed_required_job_count"
        , "disqualifying_jobs"
        , "committer_date"
        , "age_hours"
        ]
      , "FROM master_commit_job_success_completeness_mview"
      , "WHERE not_succeeded_required_job_count <= ?"
      , "AND failed_required_job_count <= ?"
      , "ORDER BY"
      , Q.list [
          "commit_id DESC"
        ]
      , "LIMIT 100"
      ]


-- | Also works for commit count lag
data ViableCommitAgeRecord a = ViableCommitAgeRecord {
    _inserted_at                                    :: UTCTime
  , _failed_required_job_count_threshold            :: Int
  , _unbuilt_or_failed_required_job_count_threshold :: Int
  , _commit_id                                      :: Int64
  , _value                                          :: a
  } deriving (Generic, FromRow)

instance (ToJSON a) => ToJSON (ViableCommitAgeRecord a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore



data TimeRange =
    Bounded (DbHelpers.StartEnd UTCTime)
  | StartOnly UTCTime


data FailuresByJobReviewCounts = FailuresByJobReviewCounts {
    _job                    :: Text
  , _isolated_failure_count :: Int
  , _recognized_flaky_count :: Int
  , _matched_count          :: Int
  , _timeout_count          :: Int
  , _min_commit_index       :: Int
  , _max_commit_index       :: Int
  , _min_commit_number      :: Int
  , _max_commit_number      :: Int
  } deriving (Generic, FromRow)

instance ToJSON FailuresByJobReviewCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


queryParmsFromTimeRange :: TimeRange -> (UTCTime, Maybe UTCTime)
queryParmsFromTimeRange time_bounds =
  case time_bounds of
    Bounded (DbHelpers.StartEnd start_time end_time) -> (start_time, Just end_time)
    StartOnly start_time -> (start_time, Nothing)


apiCoarseBinsIsolatedJobFailuresTimespan ::
     TimeRange
  -> DbIO (Either Text [WebApi.PieSliceApiRecord])
apiCoarseBinsIsolatedJobFailuresTimespan time_bounds = do
  conn <- ask
  liftIO $ do
    rows <- query conn sql query_parms
    runExceptT $ do
      (
          timeout_count
        , no_logs_count
        , network_count
        , other_count
        , unmatched_count
        , sanity_check_consistent_total
        ) <- except $ maybeToEither "No rows" $ Safe.headMay rows

      let result = [
              (WebApi.PieSliceApiRecord "Timeout" timeout_count)
            , (WebApi.PieSliceApiRecord "No logs" no_logs_count)
            , (WebApi.PieSliceApiRecord "Network" network_count)
            , (WebApi.PieSliceApiRecord "Other match" other_count)
            , (WebApi.PieSliceApiRecord "Unmatched" unmatched_count)

            ]

      unless sanity_check_consistent_total $
        except $ Left $ T.unwords [
          "Inconsisent build counts;"
        , T.pack $ show result
        ]

      return result

  where
    query_parms = queryParmsFromTimeRange time_bounds

    -- Compare to "build_failure_causes_disjoint" view for logic
    -- of categorizing mutually-exclusive failure modes
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "timeout_count"
        , "no_logs_count"
        , "network_count"
        , "other_count"
        , "unmatched_count"
        , "timeout_count + no_logs_count + network_count + other_count + unmatched_count = total_count AS sanity_check_consistent_total"
        ]
      , "FROM"
      , Q.parens inner_sql
      , "foo"
      ]

    inner_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "COALESCE(SUM(is_timeout::int), 0) AS timeout_count"
        , "COALESCE(SUM(is_idiopathic::int), 0) AS no_logs_count"
        , "COALESCE(SUM(is_network::int), 0) AS network_count"
        , "COALESCE(SUM((is_matched AND (NOT (is_network OR is_timeout)))::int), 0) AS other_count"
        , "COALESCE(SUM((NOT (is_timeout OR is_matched OR is_idiopathic))::int), 0) AS unmatched_count"
        , "COUNT(*) AS total_count"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "JOIN master_ordered_commits_with_metadata_mview"
      , "ON master_failures_raw_causes_mview.commit_index = master_ordered_commits_with_metadata_mview.id"
      , "WHERE"
      , Q.qconjunction [
          "is_serially_isolated"
        , "NOT succeeded"
        , "tstzrange(?::timestamp, ?::timestamp) @> committer_date"
        ]
      ]


apiIsolatedUnmatchedBuildsMasterCommitRange ::
     DbHelpers.InclusiveNumericBounds Int64
  -> DbIO (Either Text [WebApi.BuildBranchDateRecord])
apiIsolatedUnmatchedBuildsMasterCommitRange
    (DbHelpers.InclusiveNumericBounds lower_commit_id upper_commit_id) = do

  conn <- ask
  liftIO $ do
    rows <- query conn sql query_parms
    return $ Right rows
  where
    query_parms = (lower_commit_id, upper_commit_id)

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "master_failures_raw_causes_mview.global_build"
        , "master_failures_raw_causes_mview.step_name"
        , "master_failures_raw_causes_mview.job_name"
        , "master_failures_raw_causes_mview.sha1"
        , "master_failures_raw_causes_mview.queued_at"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "JOIN ci_providers"
      , "ON ci_providers.id = master_failures_raw_causes_mview.provider"
      , "WHERE"
      , Q.qconjunction [
          "master_failures_raw_causes_mview.is_serially_isolated"
        , "NOT master_failures_raw_causes_mview.succeeded"
        , "NOT master_failures_raw_causes_mview.is_timeout"
        , "NOT master_failures_raw_causes_mview.is_idiopathic"
        , "master_failures_raw_causes_mview.pattern_id < 0"
        , "int8range(?, ?, '[]') @> master_failures_raw_causes_mview.commit_index::int8"
        ]
      ]


apiIsolatedJobFailuresTimespan ::
     TimeRange
  -> DbIO (Either Text [FailuresByJobReviewCounts])
apiIsolatedJobFailuresTimespan time_bounds = do
  conn <- ask
  liftIO $ do
    rows <- query conn sql query_parms
    return $ Right rows
  where

    query_parms = queryParmsFromTimeRange time_bounds

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "SUM(is_serially_isolated::int) AS isolated_count"
        , "SUM(is_flaky::int) AS recognized_flaky_count"
        , "SUM(is_matched::int) AS matched_count"
        , "SUM(is_timeout::int) AS timeout_count"
        , "MIN(master_failures_raw_causes_mview.commit_index) AS min_commit_index"
        , "MAX(master_failures_raw_causes_mview.commit_index) AS max_commit_index"
        , "MIN(master_ordered_commits_with_metadata_mview.commit_number) AS min_commit_number"
        , "MAX(master_ordered_commits_with_metadata_mview.commit_number) AS max_commit_number"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "JOIN master_ordered_commits_with_metadata_mview"
      , "ON master_failures_raw_causes_mview.commit_index = master_ordered_commits_with_metadata_mview.id"
      , "WHERE"
      , Q.qconjunction [
          "is_serially_isolated"
        , "NOT succeeded"
        , Q.qjoin [
            "tstzrange(?::timestamp, ?::timestamp)"
          , "@> master_ordered_commits_with_metadata_mview.committer_date"
          ]
        ]
      , "GROUP BY job_name"
      , "ORDER BY isolated_count DESC, job_name"
      ]


data FailuresByPatternReviewCounts = FailuresByPatternReviewCounts {
    _pattern_id             :: Maybe ScanPatterns.PatternId
  , _expression             :: Maybe Text
  , _isolated_failure_count :: Int
  , _recognized_flaky_count :: Int
  , _matched_count          :: Int
  , _min_commit_index       :: Int
  , _max_commit_index       :: Int
  , _min_commit_number      :: Int
  , _max_commit_number      :: Int
  , _has_pattern_match      :: Bool
  , _is_network             :: Bool
  } deriving (Generic, FromRow)

instance ToJSON FailuresByPatternReviewCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiIsolatedPatternFailuresTimespan ::
     TimeRange
  -> DbIO (Either Text [FailuresByPatternReviewCounts])
apiIsolatedPatternFailuresTimespan time_bounds = do
  conn <- ask
  liftIO $ do

    rows <- query conn sql query_parms
    return $ Right rows
  where

    query_parms = case time_bounds of
      Bounded (DbHelpers.StartEnd start_time end_time) -> (start_time, Just end_time)
      StartOnly start_time -> (start_time, Nothing)

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "patterns_rich.id AS pattern_id"
        , "patterns_rich.expression"
        , "isolated_count"
        , "recognized_flaky_count"
        , "matched_count"
        , "min_commit_index"
        , "max_commit_index"
        , "min_commit_number"
        , "max_commit_number"
        , "patterns_rich.id IS NOT NULL AS has_pattern_match"
        , "COALESCE(patterns_rich.is_network, FALSE)"
        ]
      , "FROM"
      , Q.parens inner_sql
      , "foo"
      , "LEFT JOIN patterns_rich"
      , "ON patterns_rich.id = foo.pid"
      , "ORDER BY has_pattern_match, isolated_count DESC, pattern_id DESC"
      ]

    inner_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "pattern_id AS pid"
        , "SUM(master_failures_raw_causes_mview.is_serially_isolated::int) AS isolated_count"
        , "SUM(master_failures_raw_causes_mview.is_flaky::int) AS recognized_flaky_count"
        , "SUM(master_failures_raw_causes_mview.is_matched::int) AS matched_count"
        , "MIN(master_failures_raw_causes_mview.commit_index) AS min_commit_index"
        , "MAX(master_failures_raw_causes_mview.commit_index) AS max_commit_index"
        , "MIN(master_ordered_commits_with_metadata_mview.commit_number) AS min_commit_number"
        , "MAX(master_ordered_commits_with_metadata_mview.commit_number) AS max_commit_number"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "JOIN master_ordered_commits_with_metadata_mview"
      , "ON master_failures_raw_causes_mview.commit_index = master_ordered_commits_with_metadata_mview.id"
      , "WHERE"
      , Q.qconjunction [
          "master_failures_raw_causes_mview.is_serially_isolated"
        , "NOT master_failures_raw_causes_mview.succeeded"
        , "NOT master_failures_raw_causes_mview.is_idiopathic"
        , "NOT master_failures_raw_causes_mview.is_timeout"
        , "tstzrange(?::timestamp, ?::timestamp) @> master_ordered_commits_with_metadata_mview.committer_date"
        ]
      , "GROUP BY master_failures_raw_causes_mview.pattern_id"
      ]


apiJobFailuresInTimespan ::
     Text -- ^ job name
  -> DbHelpers.InclusiveNumericBounds Int64
  -> DbIO (Either Text [CommitBuilds.CommitBuild])
apiJobFailuresInTimespan job_name (DbHelpers.InclusiveNumericBounds lower_commit_id upper_commit_id) = do
  conn <- ask
  liftIO $ do
    rows <- query conn sql query_parms
    return $ Right rows
  where
    sql = genMasterFailureDetailsQuery "master_failures_raw_causes_mview.job_name = ?"
    query_parms = (lower_commit_id, upper_commit_id, job_name)


genMasterFailureDetailsQuery extra_where_condition =
  Q.qjoin [
        "SELECT"
      , Q.list [
          "master_failures_raw_causes_mview.step_name"
        , "master_failures_raw_causes_mview.match_id"
        , "master_failures_raw_causes_mview.build_num"
        , "master_failures_raw_causes_mview.sha1"
        , "master_failures_raw_causes_mview.queued_at"
        , "master_failures_raw_causes_mview.job_name"
        , "master_failures_raw_causes_mview.branch"
        , "master_failures_raw_causes_mview.pattern_id"
        , "master_failures_raw_causes_mview.line_number"
        , "master_failures_raw_causes_mview.line_count"
        , "master_failures_raw_causes_mview.line_text"
        , "master_failures_raw_causes_mview.span_start"
        , "master_failures_raw_causes_mview.span_end"
        , "master_failures_raw_causes_mview.specificity"
        , "master_failures_raw_causes_mview.global_build"
        , "master_failures_raw_causes_mview.provider"
        , "master_failures_raw_causes_mview.build_namespace"
        , "master_failures_raw_causes_mview.succeeded"
        , "ci_providers.label"
        , "ci_providers.icon_url"
        , "master_failures_raw_causes_mview.started_at"
        , "master_failures_raw_causes_mview.finished_at"
        , "master_failures_raw_causes_mview.is_timeout"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "JOIN ci_providers"
      , "ON ci_providers.id = master_failures_raw_causes_mview.provider"
      , "WHERE"
      , Q.qconjunction [
          "master_failures_raw_causes_mview.is_serially_isolated"
        , "NOT master_failures_raw_causes_mview.succeeded"
        , "int8range(?, ?, '[]') @> master_failures_raw_causes_mview.commit_index::int8"
        , extra_where_condition
        ]
      ]


apiPatternFailuresInTimespan ::
     ScanPatterns.PatternId
  -> DbHelpers.InclusiveNumericBounds Int64
  -> DbIO (Either Text [CommitBuilds.CommitBuild])
apiPatternFailuresInTimespan
    (ScanPatterns.PatternId pattern_id)
    (DbHelpers.InclusiveNumericBounds lower_commit_id upper_commit_id) = do

  conn <- ask
  liftIO $ do
    rows <- query conn sql query_parms
    return $ Right rows
  where
    sql = genMasterFailureDetailsQuery "master_failures_raw_causes_mview.pattern_id = ?"
    query_parms = (lower_commit_id, upper_commit_id, pattern_id)


-- | Note list reversal for the sake of Highcharts
apiLatestViableMasterCommitAgeHistory ::
     Int -- ^ weeks count
  -> UTCTime -- ^ end time
  -> DbIO [ViableCommitAgeRecord Double]
apiLatestViableMasterCommitAgeHistory weeks_count end_time = do
  conn <- ask
  liftIO $ reverse <$> query conn sql (end_time, weeks_count, end_time)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "inserted_at"
        , "failed_required_job_count_threshold"
        , "unbuilt_or_failed_required_job_count_threshold"
        , "commit_id"
        , "age_hours"
        ]
      , "FROM viable_master_commit_age_history"
      , "WHERE"
      , "tstzrange(?::timestamp - interval '? weeks', ?::timestamp) @> inserted_at"
      , "ORDER BY inserted_at DESC"
      -- Hard coding a row limit doesn't work to indirectly define a timespan,
      -- both because occasionally the records are not evenly spaced
      -- and because there are multiple records for each timestamp.
--      , "LIMIT 250"
      ]


-- | Note list reversal for the sake of Highcharts
apiLatestViableMasterCommitLagCountHistory ::
     Int -- ^ weeks count
  -> UTCTime -- ^ end time
  -> DbIO [ViableCommitAgeRecord Int]
apiLatestViableMasterCommitLagCountHistory weeks_count end_time = do
  conn <- ask
  liftIO $ reverse <$> query conn sql (end_time, weeks_count)
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "inserted_at"
        , "failed_required_job_count_threshold"
        , "unbuilt_or_failed_required_job_count_threshold"
        , "commit_id"
        , "commit_count_behind"
        ]
      , "FROM viable_master_commit_age_history"
      , "WHERE inserted_at > ?::timestamp - interval '? weeks'"
      , "AND commit_count_behind IS NOT NULL"
      , "ORDER BY inserted_at DESC"
      ]


getBreakageSpans ::
     Connection
  -> DbHelpers.InclusiveNumericBounds Int64
  -> IO [BuildResults.JobFailureSpan]
getBreakageSpans conn commit_id_bounds =
  query conn job_failure_spans_sql parms_tuple
  where
    bounds_tuple = DbHelpers.boundsAsTuple commit_id_bounds
    parms_tuple = (fst bounds_tuple, snd bounds_tuple, fst bounds_tuple, snd bounds_tuple)

    job_failure_spans_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "int8range(?, ?, '[]') * failure_commit_id_range"
        , "span_length"
        ]
      , "FROM master_job_failure_spans_conservative_mview"
      , "WHERE int8range(?, ?, '[]') && failure_commit_id_range"
      , "AND COALESCE(span_length > 1, TRUE)"
      ]


-- | Gets last N commits in one query,
-- then gets the list of jobs that apply to those commits,
-- then gets the associated builds
apiMasterBuilds ::
     Pagination.TimelineParms
  -> DbIO (Either Text (DbHelpers.BenchmarkedResponse BuildResults.DbMasterBuildsBenchmarks BuildResults.MasterBuildsResponse))
apiMasterBuilds timeline_parms = do

  last_update_time <- getLastCachedMasterGridRefreshTime

  conn <- ask
  liftIO $ runExceptT $ do

    (commits_list_time, (commit_id_bounds, master_commits)) <- MyUtils.timeThisFloat $
      ExceptT $ getMasterCommits conn $ Pagination.offset_mode timeline_parms

    let commit_bounds_tuple = DbHelpers.boundsAsTuple commit_id_bounds

    (code_breakages_time, code_breakage_ranges) <- MyUtils.timeThisFloat $ liftIO $
      runReaderT (apiAnnotatedCodeBreakages should_use_uncached_annotations commit_id_bounds) conn

    (builds_list_time, completed_builds) <- MyUtils.timeThisFloat $
      liftIO $ query conn builds_list_sql commit_bounds_tuple

    (reversion_spans_time, reversion_spans) <- MyUtils.timeThisFloat $
      liftIO $ query conn reversion_spans_sql commit_bounds_tuple

    (job_failure_spans_time, job_failure_spans) <- MyUtils.timeThisFloat $
      liftIO $ getBreakageSpans conn commit_id_bounds

    (disjoint_statuses_time, disjoint_statuses) <- MyUtils.timeThisFloat $
      liftIO $ query conn disjoint_statuses_sql commit_bounds_tuple

    let (successful_builds, failed_builds) = partition
          (BuildResults.isSuccess . BuildResults._failure_mode)
          completed_builds

        maybe_successful_column_limit = Pagination.should_suppress_fully_successful_columns $
          Pagination.column_filtering timeline_parms

        successful_job_names = Set.fromList $ map (Builds.job_name . BuildResults._build) successful_builds
        failed_job_names = Set.fromList $ map (Builds.job_name . BuildResults._build) failed_builds

        strictly_successful_jobs = Set.difference successful_job_names failed_job_names

        filtered_job_names = case maybe_successful_column_limit of
          Nothing -> Set.union failed_job_names strictly_successful_jobs
          Just total_column_cap -> let
            successful_column_cap = max 0 $ total_column_cap - Set.size failed_job_names
            in Set.union failed_job_names $ Set.fromList $ take successful_column_cap $
                 Set.toAscList strictly_successful_jobs

        timing_data = BuildResults.DbMasterBuildsBenchmarks
          builds_list_time
          commits_list_time
          code_breakages_time
          disjoint_statuses_time
          job_failure_spans_time
          reversion_spans_time
          last_update_time


    return $ DbHelpers.BenchmarkedResponse timing_data $ BuildResults.MasterBuildsResponse
      filtered_job_names
      master_commits
      completed_builds
      code_breakage_ranges
      disjoint_statuses
      job_failure_spans
      reversion_spans

  where
    suppress_scheduled_builds = Pagination.should_suppress_scheduled_builds $
      Pagination.column_filtering timeline_parms

    should_use_uncached_annotations = Pagination.should_use_uncached_annotations timeline_parms

    reversion_spans_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "reverted_commit_id"
        , "reversion_commit_id"
        ]
      , "FROM master_commit_reversion_spans_mview"
      , "WHERE int8range(?, ?, '[]') && reversion_span"
      , "ORDER BY reversion_commit_id DESC"
      ]


    -- FIXME - the query of "disjoint_circleci_build_statuses" is too slow,
    -- so we only look at "successes".
    disjoint_statuses_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "ordered_master_commits.id AS commit_id"
        , "ordered_master_commits.sha1"
        , "created_at"
        , "job_name_extracted"
        , "build_number_extracted"
        , "state"
        ]
      , "FROM github_status_events_circleci_success"
      , "JOIN ordered_master_commits"
      , "ON ordered_master_commits.sha1 = github_status_events_circleci_success.sha1"
      , "WHERE int8range(?, ?, '[]') @> ordered_master_commits.id::int8"
      ]

    {-
      , "FROM disjoint_circleci_build_statuses"
      , "WHERE int8range(?, ?, '[]') @> commit_id::int8"
      ]
    -}



    filtered_statement_parts = MyUtils.applyIf
      suppress_scheduled_builds
      (++ ["AND NOT COALESCE(maybe_is_scheduled, FALSE)"])
      statement_parts

    builds_list_sql = Q.qjoin filtered_statement_parts

    statement_parts = [
        "SELECT"
      , Q.list [
          "sha1"
        , "succeeded"
        , "is_idiopathic"
        , "is_flaky"
        , "is_timeout"
        , "is_matched"
        , "is_known_broken"
        , "build_num"
        , "queued_at"
        , "job_name"
        , "branch"
        , "step_name"
        , "pattern_id"
        , "match_id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "specificity"
        , "is_serially_isolated"
        , "started_at"
        , "finished_at"
        , "global_build"
        , "provider"
        , "build_namespace"
        , "contiguous_run_count"
        , "contiguous_group_index"
        , "contiguous_start_commit_index"
        , "contiguous_end_commit_index"
        , "contiguous_length"
        , "cluster_id"
        , "cluster_member_count"
        ]
      , "FROM master_failures_raw_causes_mview"
      , "WHERE int8range(?, ?, '[]') @> commit_index::int8"
      ]


data BreakageDateRangeSimple = BreakageDateRangeSimple {
    _pr                          :: Maybe Int
  , _foreshadowed_by_pr_failures :: Bool
  , _span                        :: DbHelpers.StartEnd UTCTime
  } deriving Generic

instance ToJSON BreakageDateRangeSimple where
  toJSON = genericToJSON JsonUtils.dropUnderscore


instance FromRow BreakageDateRangeSimple where
  fromRow = BreakageDateRangeSimple
    <$> field
    <*> field
    <*> fromRow


-- | Represents any breakage of any job on Master
data DirtyMasterSpan = DirtyMasterSpan {
    _group_index :: Int
  , _span        :: DbHelpers.StartEnd UTCTime
  } deriving Generic

instance ToJSON DirtyMasterSpan where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance FromRow DirtyMasterSpan where
  fromRow = DirtyMasterSpan
    <$> field
    <*> fromRow


data ExplorableBreakageSpans = ExplorableBreakageSpans {
    _annotated_master :: [BreakageDateRangeSimple]
  , _dirty_master     :: [DirtyMasterSpan]
  } deriving Generic

instance ToJSON ExplorableBreakageSpans where
  toJSON = genericToJSON JsonUtils.dropUnderscore


masterCommitsGranular :: DbIO ExplorableBreakageSpans
masterCommitsGranular = do
  annotated_master_spans <- runQuery $ Q.qjoin [
      "SELECT"
    , "github_pr_number, foreshadowed_by_pr_failures, start_date, end_date"
    , "FROM code_breakage_nonoverlapping_spans_dated;"
    ]

  dirty_master_spans <- runQuery $ Q.qjoin [
      "SELECT"
    , "group_index, breakage_start, breakage_end"
    , "FROM master_indiscriminate_failure_spans"
    , "ORDER BY breakage_start;"
    ]

  return $ ExplorableBreakageSpans
    annotated_master_spans
    dirty_master_spans


data JobScheduleStats = JobScheduleStats {
    _job_name                                         :: Text
  , _commit_to_build_latency_coefficient_of_variation :: Double
  , _build_interval_coefficient_of_variation          :: Double
  , _circular_time_of_day_stddev                      :: Double
  , _circular_time_of_day_average                     :: TimeOfDay
  } deriving (Generic, FromRow)

instance ToJSON JobScheduleStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiJobScheduleStats :: DbIO [JobScheduleStats]
apiJobScheduleStats = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "job_name"
    , "commit_to_build_latency_coefficient_of_variation"
    , "build_interval_coefficient_of_variation"
    , "circular_time_of_day_stddev"
    , "circular_time_of_day_average"
    ]
  , "FROM job_schedule_statistics_mview"
  , "WHERE build_count > 1;"
  ]


apiDetectedCodeBreakages :: DbIO [BuildResults.DetectedBreakageSpan]
apiDetectedCodeBreakages = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "first_commit_id"
    , "jobs"
    , "job_count"
    , "min_run_length"
    , "max_run_length"
    , "modal_run_length"
    , "min_last_commit_id"
    , "max_last_commit_id"
    , "modal_last_commit_id"
    , "first_commit"
    , "min_last_commit"
    , "max_last_commit"
    , "modal_last_commit"
    ]
  , "FROM master_contiguous_failure_blocks_with_commits"
  , "ORDER BY first_commit_id DESC;"
  ]


apiListFailureModes :: DbIO [DbHelpers.WithId BuildResults.MasterFailureModeDetails]
apiListFailureModes = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "id"
    , "label"
    , "revertible"
    ]
  , "FROM master_failure_modes ORDER BY id;"
  ]


annoatedCodeBreakagesFields = [
    "cause_id"
  , "cause_commit_index"
  , "cause_sha1"
  , "description"
  , "failure_mode_reporter"
  , "failure_mode_reported_at"
  , "failure_mode_id"
  , "cause_reporter"
  , "cause_reported_at"
  , "cause_jobs"
  , "breakage_commit_author"
  , "breakage_commit_message"
  , "breakage_commit_date"
  , "resolution_id"
  , "resolved_commit_index"
  , "resolution_sha1"
  , "resolution_reporter"
  , "resolution_reported_at"
  , "resolution_commit_author"
  , "resolution_commit_message"
  , "resolution_commit_date"
  , "spanned_commit_count"
  , "commit_timespan_seconds"
  ]


-- | Filters by commit id range
apiAnnotatedCodeBreakages ::
     Bool
  -> DbHelpers.InclusiveNumericBounds Int64
  -> DbIO [BuildResults.BreakageSpan Text ()]
apiAnnotatedCodeBreakages should_use_uncached_annotations commit_id_bounds = do
  conn <- ask
  liftIO $ query conn sql $ DbHelpers.boundsAsTuple commit_id_bounds
  where

    source_table = if should_use_uncached_annotations
      then "known_breakage_summaries_sans_impact"
      else "known_breakage_summaries_sans_impact_mview"

    sql = Q.qjoin [
        "SELECT"
      , Q.list annoatedCodeBreakagesFields
      , "FROM"
      , source_table
      , "WHERE int8range(?, ?, '[]') && commit_index_span"
      , "ORDER BY cause_commit_index DESC;"
      ]


sqlPrefixAnnotatedCodeBreakagesWithImpact = Q.qjoin [
    "SELECT"
  , Q.list $ annoatedCodeBreakagesFields ++ [
      "downstream_broken_commit_count"
    , "failed_downstream_build_count"
    , "github_pr_number"
    , "github_pr_head_commit"
    , "foreshadowed_broken_jobs_array"
    ]
  , "FROM known_breakage_summaries"
  ]


apiAnnotatedCodeBreakagesWithImpact :: DbIO [BuildResults.BreakageSpan Text BuildResults.BreakageImpactStats]
apiAnnotatedCodeBreakagesWithImpact = runQuery $ Q.qjoin [
    sqlPrefixAnnotatedCodeBreakagesWithImpact
  , "ORDER BY cause_commit_index DESC;"
  ]


-- | TODO "head" is partial
apiCodeBreakagesModeSingle ::
     Int
  -> DbIO Int
apiCodeBreakagesModeSingle cause_id = do
  conn <- ask
  liftIO $ do
    [Only mode_id] <- query conn sql (Only cause_id)
    return mode_id

  where
    sql = Q.qjoin [
        "SELECT"
      , "mode_id"
      , "FROM latest_master_failure_mode_attributions"
      , "WHERE cause_id = ?;"
      ]


apiAnnotatedCodeBreakagesWithImpactSingle ::
     Int
  -> DbIO [BuildResults.BreakageSpan Text BuildResults.BreakageImpactStats]
apiAnnotatedCodeBreakagesWithImpactSingle cause_id = do
  conn <- ask
  liftIO $ query conn sql $ Only cause_id
  where
    sql = Q.qjoin [
        sqlPrefixAnnotatedCodeBreakagesWithImpact
      , "WHERE cause_id = ?;"
      ]


apiBreakageAuthorStats :: DbIO [BuildResults.BreakageAuthorStats]
apiBreakageAuthorStats = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "breakage_commit_author"
    , "distinct_breakage_count"
    , "cumulative_breakage_duration_seconds"
    , "cumulative_downstream_affected_commits"
    , "cumulative_spanned_master_commits"
    ]
  , "FROM upstream_breakage_author_stats"
  , "ORDER BY distinct_breakage_count DESC;"
  ]


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
    sql = Q.qjoin [
        "SELECT ordered_master_commits.sha1"
      , "FROM ordered_master_commits"
      , "LEFT JOIN commit_metadata"
      , "ON ordered_master_commits.sha1 = commit_metadata.sha1"
      , "WHERE commit_metadata.sha1 IS NOT NULL"
      , "ORDER BY ordered_master_commits.id DESC"
      , "LIMIT 1;"
      ]


getStepIdFromUniversalBuild ::
     Builds.UniversalBuildId
  -> DbIO (Either Text Builds.BuildStepId)
getStepIdFromUniversalBuild (Builds.UniversalBuildId ubuild_id) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only ubuild_id
    return $ maybeToEither err_msg $ Safe.headMay $
      map (\(Only x) -> Builds.NewBuildStepId x) xs
  where
    err_msg = T.unwords [
        "build"
      , T.pack $ show ubuild_id
      , "not in database"
      ]

    sql = Q.qjoin [
        "SELECT id"
      , "FROM build_steps"
      , "WHERE universal_build = ?"
      ]


data ScanTestResponse = ScanTestResponse {
    _total_line_count :: Int
  , _matches          :: [ScanPatterns.ScanMatch]
  } deriving Generic

instance ToJSON ScanTestResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


retrieveLogFromBuildId ::
     Builds.UniversalBuildId
  -> DbIO (Either Text LT.Text)
retrieveLogFromBuildId universal_build_id = runExceptT $ do
  step_id <- ExceptT $ getStepIdFromUniversalBuild universal_build_id
  ExceptT $ do
    maybe_log <- readLog step_id
    return $ maybeToEither ("log not in database" :: Text) maybe_log


-- TODO consolidate with Scanning.scan_log
apiNewPatternTest ::
     Builds.UniversalBuildId
  -> ScanPatterns.Pattern
  -> DbIO (Either Text ScanTestResponse)
apiNewPatternTest universal_build_id new_pattern =

  runExceptT $ do
    console_log <- ExceptT $ retrieveLogFromBuildId universal_build_id

    let mylines = LT.lines console_log
    return $ ScanTestResponse (length mylines) $
      Maybe.mapMaybe apply_pattern $ zip [0::Int ..] $
        map LT.stripEnd mylines

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


makePatternRecords =
  map $ \(a, b, c, d, e, f, g, h, i, j, k) ->
    PatternRecord a b c d e f g (DbHelpers.splitAggText h) (DbHelpers.splitAggText i) j k


-- | Returns zero or one pattern.
apiSinglePattern :: ScanPatterns.PatternId -> DbIO [PatternRecord]
apiSinglePattern (ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ fmap makePatternRecords $ query conn sql $ Only pattern_id
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "id"
        , "regex"
        , "expression"
        , "description"
        , "matching_build_count"
        , "most_recent"
        , "earliest"
        , "tags"
        , "steps"
        , "specificity"
        , "CASE total_scanned_builds WHEN 0 THEN 0 ELSE CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) END AS percent_scanned"
        ]
      , "FROM pattern_frequency_summary_partially_cached"
      , "WHERE id = ?;"
      ]


apiPatterns :: DbIO [PatternRecord]
apiPatterns = fmap makePatternRecords $ runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "id"
    , "regex"
    , "expression"
    , "description"
    , "matching_build_count"
    , "most_recent"
    , "earliest"
    , "tags"
    , "steps"
    , "specificity"
    , "CASE total_scanned_builds WHEN 0 THEN 0 ELSE CAST((scanned_count * 100 / total_scanned_builds) AS DECIMAL(6, 1)) END AS percent_scanned"
    ]
  , "FROM pattern_frequency_summary_partially_cached"
  , "ORDER BY most_recent DESC NULLS LAST;"
  ]


-- | For the purpose of database upgrades
dumpPatterns :: DbIO [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
dumpPatterns = map f <$> runQuery q

  where
    q = Q.qjoin [
        "SELECT"
      , Q.list [
          "author"
        , "created"
        , "id"
        , "regex"
        , "expression"
        , "has_nondeterministic_values"
        , "description"
        , "tags"
        , "steps"
        , "specificity"
        , "is_retired"
        , "lines_from_end"
        ]
      , "FROM patterns_augmented"
      , "ORDER BY id;"
      ]


    split_texts = sort . map T.pack . DbHelpers.splitAggText

    f (author, created, pattern_id, is_regex, expression, has_nondeterministic_values, description, tags, steps, specificity, is_retired, lines_from_end) =
      DbHelpers.WithAuthorship author created $ wrapPattern pattern_id is_regex expression has_nondeterministic_values description
        (split_texts tags)
        (split_texts steps)
        specificity
        is_retired
        lines_from_end


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
  -> DbIO (DbHelpers.BenchmarkedResponse Float [MatchOccurrences.MatchOccurrencesForBuild])
getBuildPatternMatches (Builds.UniversalBuildId build_id) = do
  conn <- ask
  (timing, result) <- MyUtils.timeThisFloat $ liftIO $ query conn sql $ Only build_id
  return $ DbHelpers.BenchmarkedResponse timing result
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "step_name"
        , "pattern"
        , "matches_with_log_metadata.id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "specificity"
        ]
      , "FROM matches_with_log_metadata"
      , "JOIN build_steps_deduped_mitigation"
      , "ON matches_with_log_metadata.build_step = build_steps_deduped_mitigation.id"
      , "JOIN patterns_augmented"
      , "ON patterns_augmented.id = matches_with_log_metadata.pattern"
      , "WHERE build_steps_deduped_mitigation.universal_build = ?"
      , "ORDER BY specificity DESC, patterns_augmented.id ASC, line_number ASC;"
      ]


data StorageStats = StorageStats {
    _total_lines :: Integer
  , _total_bytes :: Integer
  , _log_count   :: Integer
  } deriving (Generic, FromRow)

instance ToJSON StorageStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | FIXME partial head
apiStorageStats :: DbIO StorageStats
apiStorageStats = fmap head $ runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "SUM(line_count) AS total_lines"
    , "SUM(byte_count) AS total_bytes"
    , "COUNT(*) log_count"
    ]
  , "FROM log_metadata;"
  ]


patternOccurrenceTxForm pattern_id = f
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


commonQueryPrefixPatternMatches :: Query
commonQueryPrefixPatternMatches = Q.qjoin [
    "SELECT"
  , Q.list [
      "build"
    , "step_name"
    , "match_id"
    , "line_number"
    , "line_count"
    , "line_text"
    , "span_start"
    , "span_end"
    , "vcs_revision"
    , "queued_at"
    , "job_name"
    , "branch"
    , "universal_build"
    ]
  , "FROM best_pattern_match_augmented_builds"
  , "WHERE pattern_id = ?"
  ]


-- | Limit is arbitrary
getBestPatternMatches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getBestPatternMatches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (patternOccurrenceTxForm pat) <$> query conn sql (Only pattern_id)

  where
    sql = Q.qjoin [
        commonQueryPrefixPatternMatches
      , "LIMIT 100;"
      ]


getBestPatternMatchesWhitelistedBranches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getBestPatternMatchesWhitelistedBranches pat@(ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ map (patternOccurrenceTxForm pat) <$> query conn sql (Only pattern_id)
  where
    sql = Q.qjoin [
        commonQueryPrefixPatternMatches
      , "AND branch IN (SELECT branch from presumed_stable_branches);"
      ]


getPostedGithubStatus ::
     DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> DbIO (Maybe (Text, Text))
getPostedGithubStatus
    (DbHelpers.OwnerAndRepo project repo)
    (Builds.RawCommit sha1) = do

  conn <- ask
  liftIO $ do
    putStrLn "Inside getPostedGithubStatus..."
    xs <- query conn sql (sha1, project, repo)
    putStrLn "Finishing getPostedGithubStatus..."
    return $ Safe.headMay xs

  where
    sql = Q.qjoin [
        "SELECT"
      , "state, description"
      , "FROM created_github_statuses"
      , "WHERE sha1 = ? AND project = ? AND repo = ?"
      , "ORDER BY id DESC LIMIT 1;"
      ]


-- | This should produce one or zero results.
-- We use a list instead of a Maybe so that
-- the javascript table renderer code can be reused
-- for multi-item lists.
getBestBuildMatch ::
     Builds.UniversalBuildId
  -> DbIO (DbHelpers.BenchmarkedResponse Float [PatternOccurrence])
getBestBuildMatch ubuild_id@(Builds.UniversalBuildId build_id) = do

  conn <- ask
  (timing, content) <- MyUtils.timeThisFloat $ liftIO $
    map f <$> query conn sql (Only build_id)

  return $ DbHelpers.BenchmarkedResponse timing content

  where
    f ( pattern_id
      , build
      , step_name
      , match_id
      , line_number
      , line_count
      , line_text
      , span_start
      , span_end
      , vcs_revision
      , queued_at
      , job_name
      , branch) = patternOccurrenceTxForm
        (ScanPatterns.PatternId pattern_id)
        ( build
        , step_name
        , match_id
        , line_number
        , line_count
        , line_text
        , span_start
        , span_end
        , vcs_revision
        , queued_at
        , job_name
        , branch
        , ubuild_id)

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "pattern_id"
        , "build"
        , "step_name"
        , "match_id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        ]
      , "FROM best_pattern_match_augmented_builds"
      , "WHERE universal_build = ?;"
      ]


data LogContext = LogContext {
    _match_info :: ScanPatterns.MatchDetails
  , _log_lines  :: [(Int, LT.Text)]
  } deriving Generic

instance ToJSON LogContext where
  toJSON = genericToJSON JsonUtils.dropUnderscore


hiddenContextLinecount :: Int
hiddenContextLinecount = 1000


-- | TODO: Could shave off a bit more time
-- by doing the line count arithmetic in Postgres
logContextFunc ::
     MatchOccurrences.MatchId
  -> Int
  -> DbIO (Either Text LogContext)
logContextFunc mid@(MatchOccurrences.MatchId match_id) context_linecount = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only match_id
    let maybe_first_row = Safe.headMay xs

    runExceptT $ do

      first_row <- except $ maybeToEither errmsg maybe_first_row

      let (line_number, span_start, span_end, line_text) = first_row
          match_info = ScanPatterns.NewMatchDetails line_text line_number $ DbHelpers.StartEnd span_start span_end

          first_context_line = max 0 $ line_number - context_linecount - hiddenContextLinecount

          last_context_line = line_number + context_linecount + 1
          retrieval_line_count = last_context_line - first_context_line

      log_lines <- liftIO $ runReaderT
        (SqlRead.readLogSubset mid first_context_line retrieval_line_count)
        conn

      let tuples = zip [first_context_line..] log_lines

      return $ LogContext
        match_info
        tuples

  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "line_number"
        , "span_start"
        , "span_end"
        , "line_text"
        ]
      , "FROM matches"
      , "WHERE id = ?;"
      ]

    errmsg = T.pack $ unwords [
        "Match ID"
      , show match_id
      , "not found"
      ]


getPatternMatches :: ScanPatterns.PatternId -> DbIO [PatternOccurrence]
getPatternMatches pattern_id =
  map f <$> getPatternOccurrenceRows pattern_id
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
        (ScanPatterns.NewMatchDetails line_text line_number (DbHelpers.StartEnd start end)) = match_details


getPatternOccurrenceRows ::
     ScanPatterns.PatternId
  -> DbIO [(Builds.Build, Text, Int, MatchOccurrences.MatchId, ScanPatterns.MatchDetails, Builds.UniversalBuildId)]
getPatternOccurrenceRows (ScanPatterns.PatternId pattern_id) = do

  conn <- ask
  liftIO $ fmap (map f) $ query conn sql $ Only pattern_id

  where
    f ( buildnum
      , stepname
      , match_id
      , line_number
      , line_count
      , line_text
      , span_start
      , span_end
      , vcs_revision
      , queued_at
      , job_name
      , branch
      , global_build_num
      , maybe_started_at
      , maybe_finished_at) =
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
            DbHelpers.StartEnd span_start span_end

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "global_builds.build_number"
        , "step_name"
        , "matches_with_log_metadata.id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "global_builds.vcs_revision"
        , "queued_at"
        , "job_name"
        , "branch"
        , "global_build_num"
        , "global_builds.started_at"
        , "global_builds.finished_at"
        ]
      , "FROM matches_with_log_metadata"
      , "JOIN global_builds"
      , "ON matches_with_log_metadata.universal_build = global_builds.global_build_num"
      , "WHERE pattern = ?;"
      ]
