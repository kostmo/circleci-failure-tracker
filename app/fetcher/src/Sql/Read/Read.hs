{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Sql.Read.Read where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       except, runExceptT)
import           Control.Monad.Trans.Reader           (ask, runReaderT)
import           Data.Aeson
import           Data.Either.Utils                    (maybeToEither)
import           Data.List                            (partition, sortOn)
import qualified Data.Maybe                           as Maybe
import           Data.Scientific                      (Scientific)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as LT
import           Data.Time                            (UTCTime)
import           Data.Time.LocalTime                  (TimeOfDay)
import           Data.Tuple                           (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.FromRow   (field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           Database.PostgreSQL.Simple.Types     (PGArray, fromPGArray)
import           GHC.Generics
import           GHC.Int                              (Int64)
import qualified Safe

import qualified AuthStages
import qualified BuildResults
import qualified Builds
import qualified CommitBuilds
import qualified Commits
import qualified DbHelpers
import qualified DebugUtils                           as D
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified MyUtils
import qualified Pagination
import qualified PostedComments
import qualified ScanPatterns
import qualified ScanUtils
import qualified Sql.QueryUtils                       as Q
import           Sql.Read.Types                       (DbIO, runQuery)
import qualified Sql.Read.Types                       as SqlReadTypes
import qualified UnmatchedBuilds
import qualified WebApi
import qualified WeeklyStats

constructExpression ::
     Bool
  -> Text
  -> Bool
  -> ScanPatterns.MatchExpression
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
wrapPattern
    pattern_id
    is_regex
    pattern_text
    is_nondeterministic
    description
    tags_list
    steps_list
    specificity
    is_retired
    maybe_lines_from_end =

  DbHelpers.WithId pattern_id inner_pattern
  where
    expression_obj = constructExpression is_regex pattern_text is_nondeterministic

    inner_pattern = ScanPatterns.NewPattern
      expression_obj
      description
      tags_list
      steps_list
      specificity
      is_retired
      maybe_lines_from_end


-- | Returned in descending order by ID
getPatternsDescById :: Connection -> IO [ScanPatterns.DbPattern]
getPatternsDescById conn = do

  patterns_rows <- query_ conn patterns_sql

  return $ flip map patterns_rows $
    \(pattern_id, is_regex, pattern_text, has_nondeterministic_values, description, specificity, is_retired, lines_from_end, tags_list, steps_list) ->

      wrapPattern
        pattern_id
        is_regex
        pattern_text
        has_nondeterministic_values
        description
        (fromPGArray tags_list)
        (fromPGArray steps_list)
        specificity
        is_retired
        lines_from_end

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
        , "tags_array"
        , "steps_array"
        ]
      , "FROM patterns_rich"
      , "ORDER BY id DESC"
      ]


-- | Only searches for CircleCI builds
getUnvisitedBuildIds ::
     Connection
  -> Either (Set Builds.UniversalBuildId) Int
  -> IO [DbHelpers.WithId Builds.UniversalBuild]
getUnvisitedBuildIds
    conn
    whitelisted_builds_or_fetch_count = do

  rows <- case whitelisted_builds_or_fetch_count of
    Left whitelist_set -> do
      D.debugList [
          "Querying for unvisited builds in whitelist with SQL:"
        , show sql
        ]
      query conn sql
        (SqlReadTypes.circleCIProviderIndex, In $ map (\(Builds.UniversalBuildId x) -> x) $ Set.toList whitelist_set)
      where
        sql = Q.qjoin [
            sql_prefix
          , "WHERE"
          , Q.qconjunction where_conditions
          ]

        where_conditions = [
            "provider = ?"
          , "universal_build_id IN ?"
          ]

    Right limit -> query conn sql
        (SqlReadTypes.circleCIProviderIndex, limit)
      where
        sql = Q.qjoin [
            sql_prefix
          , "WHERE"
          , Q.qconjunction where_conditions
          , "ORDER BY build_num DESC"
          , "LIMIT ?"
          ]

        where_conditions = [
            "provider = ?"
          ]

  return $ map f rows

  where
    f (universal_build_id, provider_buildnum, provider_id, build_namespace, succeeded, sha1) = DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber provider_buildnum)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit sha1)

    sql_prefix = Q.qjoin [
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
      ]


-- | Only searches for CircleCI builds
getUnvisitedBuildsForSha1 ::
     Builds.RawCommit
  -> DbIO [DbHelpers.WithId Builds.UniversalBuild]
getUnvisitedBuildsForSha1 (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ do
    rows <- query conn sql (SqlReadTypes.circleCIProviderIndex, sha1)
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
      , "WHERE"
      , Q.qconjunction [
          "provider = ?"
        , "commit_sha1 = ?"
        , "NOT succeeded"
        ]
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
      , "FROM global_builds"
      , "WHERE build_number = ?"
      , "ORDER BY"
      , Q.list [
          "provider DESC"
        , "global_build_num DESC"
        ]
      , "LIMIT 1"
      ]


lookupUniversalBuild ::
     Builds.UniversalBuildId
  -> DbIO (Either Text (DbHelpers.WithId Builds.UniversalBuild))
lookupUniversalBuild (Builds.UniversalBuildId universal_build_num) = do
  conn <- ask
  xs <- liftIO $ query conn sql $ Only universal_build_num
  return $ maybeToEither "build not found" $ Safe.headMay xs
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
        ]
      , "FROM global_builds"
      , "WHERE global_build_num = ?"
      ]


getGlobalBuild ::
     Builds.UniversalBuildId
  -> DbIO (Either Text Builds.StorableBuild)
getGlobalBuild (Builds.UniversalBuildId global_build_num) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only global_build_num
    return $ maybeToEither err_msg $ Safe.headMay xs
  where
    err_msg = T.pack $ unwords [
        "Universal build number not located:"
      , show global_build_num
      ]

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
      , "FROM global_builds"
      , "WHERE global_build_num = ?"
      ]


getFlakyMasterBuildsToRetry ::
  DbIO [(Builds.UniversalBuildId, Builds.BuildNumber)]
getFlakyMasterBuildsToRetry = do
  conn <- ask
  liftIO $ query_ conn sql
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "global_build"
        , "build_num"
        ]
      , "FROM master_flaky_builds_to_retry"
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
      , "WHERE pr_number = ?"
      ]


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
      , "FROM"
      , Q.aliasedSubquery "SELECT ? AS myname" "foo"
      , "LEFT JOIN pr_comment_posting_opt_outs"
      , "ON foo.myname = pr_comment_posting_opt_outs.username"
      , "LIMIT 1"
      ]


userOptOutSettings ::
  SqlReadTypes.AuthDbIO (Either Text (UserWrapper (Maybe OptOutResponse)))
userOptOutSettings = do
  SqlReadTypes.AuthConnection conn user@(AuthStages.Username author) <- ask
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
      , "WHERE username = ?"
      ]


transformPatternRows row =
  (tup1, fromPGArray pattern_ids_array)
  where

    (   pattern_ids_array
      , step_id
      , step_name
      , universal_build_id
      , build_num
      , provider_id
      , build_namespace
      , succeeded
      , vcs_revision
      ) = row

    ubuild = DbHelpers.WithId universal_build_id $ Builds.UniversalBuild
      (Builds.NewBuildNumber build_num)
      provider_id
      build_namespace
      succeeded
      (Builds.RawCommit vcs_revision)

    tup1 = (Builds.NewBuildStepId step_id, step_name, ubuild)


getRevisitableWhitelistedBuilds ::
     [Builds.UniversalBuildId]
  -> DbIO [((Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild), [Int64])]
getRevisitableWhitelistedBuilds universal_build_ids = do
  conn <- ask
  liftIO $ do
    (timing, xs) <- D.timeThisFloat $ query conn sql $ Only $ In parms
    D.debugList [
        "getRevisitableWhitelistedBuilds took"
      , show timing
      , "seconds"
      ]

    return $ map transformPatternRows xs
  where
    parms = map (\(Builds.UniversalBuildId x) -> x) universal_build_ids
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "unscanned_patterns_array"
        , "step_id"
        , "step_name"
        , "universal_build"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        ]
      , "FROM unscanned_patterns"
      , "WHERE universal_build IN ?"
      ]


getRevisitableBuilds ::
  DbIO [((Builds.BuildStepId, Text, DbHelpers.WithId Builds.UniversalBuild), [Int64])]
getRevisitableBuilds = do
  conn <- ask
  liftIO $ map transformPatternRows <$> query_ conn sql
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "unscanned_patterns_array"
        , "step_id"
        , "step_name"
        , "universal_build"
        , "build_num"
        , "provider"
        , "build_namespace"
        , "succeeded"
        , "vcs_revision"
        ]
      , "FROM unscanned_patterns"
      ]


data PullRequestBuildStats = PullRequestBuildStats {
    _pr_number                   :: Builds.PullRequestNumber
  , _commit_id                   :: Int64
  , _master_commit               :: Builds.RawCommit
  , _pr_head_commit              :: Builds.RawCommit
  , _committer_date              :: UTCTime
  , _total_builds                :: Int
  , _succeeded_count             :: Int
  , _failed_count                :: Int
  , _foreshadowed_breakage_count :: Int
  , _all_succeeded               :: Bool
  } deriving (FromRow, Generic)

instance ToJSON PullRequestBuildStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiPrBatchList ::
     [Int64]
  -> DbIO [PullRequestBuildStats]
apiPrBatchList pr_numbers = do
  conn <- ask
  liftIO $ query conn sql $ Only $ In pr_numbers
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "github_pr_number"
        , "commit_id"
--        , "commit_number"
        , "master_commit"
        , "pr_head_commit"
        , "committer_date"
        , "total_builds"
        , "succeeded_count"
        , "failed_count"
        , "foreshadowed_breakage_count"
        , "total_builds = succeeded_count AS all_succeeded"
        ]
      , "FROM pr_merge_time_build_stats_by_master_commit_mview"
      , "WHERE"
      , "github_pr_number IN ?"
      , "ORDER BY commit_id DESC"
      ]


prCommentSqlPrefix = Q.qjoin [
    "SELECT"
  , Q.list [
      "pr_number"
    , "sha1"
    , "github_user_login"
    , "body"
    , "created_at"
    , "updated_at"
    , "revision_count"
    , "comment_id"
    , "was_new_push"
    , "all_no_fault_failures"
    , "all_successful_circleci_builds"
    , "EXTRACT(EPOCH FROM queue_residency_duration)"
    , "EXTRACT(EPOCH FROM execution_duration)"
    ]
  , "FROM lambda_logging.latest_pr_comment_revision_processing_stats"
  ]


apiPostedCommentsForPR ::
     Builds.PullRequestNumber
  -> DbIO [PostedComments.PostedComment]
apiPostedCommentsForPR (Builds.PullRequestNumber pr_number) = do
  conn <- ask
  liftIO $ query conn sql $ Only pr_number
  where
    sql = Q.qjoin [
        prCommentSqlPrefix
      , "WHERE pr_number = ?"
      , "ORDER BY updated_at DESC"
      ]


apiPostedPRComments ::
     Int
  -> DbIO [PostedComments.PostedComment]
apiPostedPRComments count = do
  conn <- ask
  liftIO $ query conn sql $
    Only $ min count SqlReadTypes.maxApiPrCommentRevisionsToFetch
  where
    sql = Q.qjoin [
        prCommentSqlPrefix
      , "ORDER BY updated_at DESC"
      , "LIMIT ?"
      ]


data WeeklyFailingMergedPullRequests = WeeklyFailingMergedPullRequests {
    _total_pr_count              :: Int
  , _failing_pr_count            :: Int
  , _total_build_count           :: Int
  , _total_failed_build_count    :: Int
  , _foreshadowed_breakage_count :: Int
  , _pr_numbers                  :: PGArray Int
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
        , "pr_numbers"
        ]
      , "FROM pr_merge_time_failing_builds_by_week"
      , "WHERE failing_pr_count IS NOT NULL"
      , "ORDER BY week DESC"
      , "OFFSET 1"
      , "LIMIT ?"
      ]


data PageRequestCounts = PageRequestCounts {
    _page_url :: Text
  , _count    :: Int
  } deriving (Generic, FromRow)

instance ToJSON PageRequestCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | Offsets so we only obtain full weeks of data.
--
-- Note also the list order reversal for Highcharts
getPageViewsByWeek ::
     Int
  -> DbIO [DbHelpers.TimestampedDatum PageRequestCounts]
getPageViewsByWeek _week_count = do
  conn <- ask
  liftIO $ reverse <$> query conn sql (Only url_count)
  where
    url_count = 8 :: Int

    top_pages_subquery = Q.qjoin [
        "SELECT"
      , Q.list [
          "url"
        , "SUM(request_count) AS total"
        ]
      , "FROM frontend_logging.page_requests_by_week"
      , "GROUP BY url"
      , "ORDER BY total DESC"
      , "LIMIT ?"
      ]

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "week"
        , "f.url"
        , "request_count"
        ]
      , "FROM frontend_logging.page_requests_by_week f"
      , "JOIN"
      , Q.aliasedSubquery top_pages_subquery "top_pages_subquery"
      , "ON f.url = top_pages_subquery.url"
      , "WHERE week IN"
      , Q.parens $ Q.qjoin [
          "SELECT DISTINCT week"
        , "FROM frontend_logging.page_requests_by_week"
        , "ORDER BY week DESC"
        , "OFFSET 1"
        ]
      , "ORDER BY week DESC"
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
      , "FROM master_failures_raw_causes_mview"
      , "GROUP BY"
      , Q.list [
          "pattern_id"
        , "week"
        ]
      ]


data TestFailure = TestFailure {
    _sha1       :: Builds.RawCommit
  , _test_name  :: Text
  , _build_date :: UTCTime
  } deriving Generic

instance ToJSON TestFailure where
  toJSON = genericToJSON JsonUtils.dropUnderscore


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
      , "WHERE pattern = ?"
      , "ORDER BY"
      , Q.list [
          "occurrence_count DESC"
        , "name ASC"
        ]
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
      , "ORDER BY"
      , Q.list [
          "occurrence_count DESC"
        , "job_name ASC"
        ]
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
      , "GROUP BY bin"
      , "ORDER BY bin ASC"
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
      , "GROUP BY bin"
      , "ORDER BY bin ASC"
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
      , "WHERE"
      , Q.qconjunction [
          "sha1 = ?"
        , "NOT succeeded"
        ]
      , "ORDER BY job_name"
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
      , "FROM master_failures_raw_causes_mview"
      , "WHERE"
      , Q.qconjunction [
          "int8range(?, ?, '[]') @> commit_index::int8"
        , "NOT succeeded"
        ]
      , "ORDER BY job_name"
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
      , "WHERE"
      , "id > " <> Q.parens subquery
      , "ORDER BY id ASC"
      , "LIMIT 1"
      ]

    subquery = Q.qjoin [
        "SELECT id"
      , "FROM ordered_master_commits"
      , "WHERE sha1 = ?"
      ]


apiJobs :: DbIO (WebApi.ApiResponse WebApi.JobApiRecord)
apiJobs =
  WebApi.ApiResponse . map f <$> runQuery q
  where
    q = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "freq"
        ]
      , "FROM job_failure_frequencies"
      ]

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
    , "FROM master_failures_raw_causes_mview"
    , "WHERE"
    , Q.qconjunction [
        "step_name IS NOT NULL"
      , "step_name != ''"
      ]
    , "GROUP BY step_name"
    , "ORDER BY freq DESC"
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
    , "FROM"
    , Q.aliasedSubquery inner_q "foo"
    , "JOIN master_failure_modes"
    , "ON foo.failure_mode_id = master_failure_modes.id"
    ]

  inner_q = Q.qjoin [
      "SELECT"
      , Q.list [
          "failure_mode_id"
        , "COUNT(*) AS freq"
        ]
    , "FROM known_breakage_summaries"
    , "GROUP BY failure_mode_id"
    , "ORDER BY freq DESC"
    ]


data DownstreamCommitInfo = DownstreamCommitInfo {
    _sha1      :: Builds.RawCommit
  , _distance  :: Int
  , _pr_number :: Maybe Int
  } deriving (Generic, FromRow)

instance ToJSON DownstreamCommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiMasterDownstreamCommits ::
     Builds.RawCommit
  -> DbIO [DownstreamCommitInfo]
apiMasterDownstreamCommits (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ query conn sql $ Only sha1
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "branch_commit"
      , "distance"
      , "pr_number"
      ]
    , "FROM pr_merge_bases"
    , "WHERE master_commit = ?"
    , "ORDER BY pr_number IS NULL"
    ]



data ThroughputHourStats = ThroughputHourStats {
    _first_inserted_hour                    :: UTCTime
  , _enqueued_count                         :: Int
  , _dequeued_count                         :: Int
  , _completed_count                        :: Int
  , _average_duration                       :: Maybe Double
  , _average_first_queue_residence_timespan :: Maybe Double
  } deriving (Generic, FromRow)

instance ToJSON ThroughputHourStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | Note that Highcharts expects the dates to be in ascending order
-- thus, use of reverse
apiThroughputByHour ::
     Int
  -> DbIO (WebApi.ApiResponse ThroughputHourStats)
apiThroughputByHour hours = do
  conn <- ask
  liftIO $ WebApi.ApiResponse . reverse <$> query conn sql (Only hours)
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "first_inserted_hour"
      , "enqueued_count"
      , "dequeued_count"
      , "completed_count"
      , "EXTRACT(EPOCH FROM average_duration)"
      , "EXTRACT(EPOCH FROM average_first_queue_residence_timespan)"
      ]
    , "FROM lambda_logging.throughput_by_hour"
    , "WHERE first_inserted_hour > now() - interval '? hours'"
    , "ORDER BY first_inserted_hour DESC"
    , "OFFSET 1"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
apiQueueDepthTimeplot ::
     Int
  -> DbIO (WebApi.ApiResponse (UTCTime, Int))
apiQueueDepthTimeplot hours = do
  conn <- ask
  liftIO $ WebApi.ApiResponse <$> query conn sql (Only hours)
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "inserted_at"
      , "queue_depth"
      ]
    , "FROM lambda_logging.sqs_queue_depth_history"
    , "WHERE inserted_at > now() - interval '? hours'"
    , "ORDER BY inserted_at"
    ]


-- | Note that Highcharts expects the dates to be in ascending order
-- thus, use of reverse
apiStatusNotificationsByHour ::
     Int
  -> DbIO (WebApi.ApiResponse (UTCTime, Int))
apiStatusNotificationsByHour hours = do
  conn <- ask
  liftIO $ WebApi.ApiResponse . reverse <$> query conn sql (Only hours)
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "date_trunc('hour', created_at) AS hour"
      , "COUNT(*)"
      ]
    , "FROM github_incoming_status_events"
    , "GROUP BY hour ORDER BY hour DESC"
    , "OFFSET 1"
    , "LIMIT ?"
    ]


data ViabilityIncreaseDatum = ViabilityIncreaseDatum {
    commit_week                      :: UTCTime
  , first_commit_sha1                :: Builds.RawCommit
  , last_commit_sha1                 :: Builds.RawCommit
  , total_commits                    :: Int
  , viable_commit_count              :: Int
  , viable_without_empirically_flaky :: Int
  , had_auto_triggered_rebuild       :: Bool
  , viable_fraction                  :: Double
  , increase_in_viability            :: Maybe Double
  } deriving (Generic, FromRow, ToJSON)


-- | Note that Highcharts expects the dates to be in ascending order
-- thus, use of reverse
apiViabilityIncreaseByWeek :: Int -> DbIO (WebApi.ApiResponse ViabilityIncreaseDatum)
apiViabilityIncreaseByWeek weeks = do
  conn <- ask
  liftIO $ WebApi.ApiResponse . reverse <$> query conn sql (Only weeks)
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "commit_week"
      , "first_commit_sha1"
      , "last_commit_sha1"
      , "total_commits"
      , "viable_commit_count"
      , "viable_without_empirically_flaky"
      , "had_auto_triggered_rebuild_count > 0"
      , "viable_fraction"
      , "increase_in_viability"
      ]
    , "FROM viability_increase_by_week"
    , "LIMIT ?"
    ]



-- | Skips the most recent week for accuracy
-- Reversed from descending order because Highcharts expects
-- the dates to be in ascending order
prCommentRevisionsByWeek :: DbIO [DbHelpers.TimestampedDatum (DbHelpers.FieldAsRowWrapper Int)]
prCommentRevisionsByWeek = do
  conn <- ask
  liftIO $ do
    xs <- query_ conn sql
    return $ reverse xs
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "date_trunc('week', updated_at) AS week"
        , "COUNT(*) AS occurrences"
        ]
      , "FROM created_pull_request_comment_revisions"
      , "GROUP BY week"
      , "ORDER BY week DESC"
      , "OFFSET 1"
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
  , Q.list [
      "branch"
    , "global_build_num"
    ]
  , "FROM idiopathic_build_failures"
  , "ORDER BY global_build_num DESC"
  ]


apiUnmatchedCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [UnmatchedBuilds.UnmatchedBuild])
apiUnmatchedCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only sha1
    return $ Right xs
  where
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
      , "WHERE unattributed_failed_builds.vcs_revision = ?"
      ]


apiIdiopathicCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [UnmatchedBuilds.UnmatchedBuild])
apiIdiopathicCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ Right . map f <$> query conn sql (Only sha1)
  where
    f (build, step_name, queued_at, job_name, branch, universal_build_id, provider_icon_url, provider_label) =
      UnmatchedBuilds.UnmatchedBuild
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
      , "WHERE vcs_revision = ?"
      ]


apiTimeoutCommitBuilds ::
     Builds.RawCommit
  -> DbIO (Either Text [UnmatchedBuilds.UnmatchedBuild])
apiTimeoutCommitBuilds (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ fmap Right $ query conn sql $ Only sha1
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "build_num"
        , "step_name"
        , "queued_at"
        , "job_name"
        , "branch"
        , "universal_build"
        , "ci_providers.icon_url"
        , "ci_providers.label"
        ]
      , "FROM builds_join_steps"
      , "JOIN ci_providers"
      , "ON builds_join_steps.provider = ci_providers.id"
      , "WHERE"
      , Q.qconjunction [
          "vcs_revision = ?"
        , "is_timeout"
        ]
      ]


-- | Obtains subset of console log from database
readLogSubset ::
     MatchOccurrences.MatchId
  -> Int -- ^ context count
  -> Int -- ^ hidden leading line count
  -> DbIO (Either Text (ScanPatterns.MatchDetails, CommitBuilds.ExcerptLinesAndStartNumber))
readLogSubset
    (MatchOccurrences.MatchId match_id)
    context_count
    hidden_leading_line_count = do

  conn <- ask
  xs <- liftIO $
    query conn sql query_parms

  let get_row (line_text, line_number, span_start, span_end, lines_array, first_line_number) = (ScanPatterns.NewMatchDetails line_text line_number $ DbHelpers.StartEnd span_start span_end, CommitBuilds.ExcerptLinesAndStartNumber (fromPGArray lines_array) first_line_number)
  return $ maybeToEither err_msg $ Safe.headMay $ map get_row xs

  where
    err_msg = T.unwords [
        "Couldn't find log for match ID"
      , T.pack $ show match_id
      ]

    query_parms = (context_count, hidden_leading_line_count, match_id)

    -- XXX This query logic is duplicated in getRevisionBuilds
    sql = Q.qjoin [
        "WITH myconstants"
      , "(context_count, hidden_leading_line_count) as (values (?, ?))"
      , "SELECT"
      , Q.list $ match_details_fields ++ [
          "array_output[first_context_line + 1:last_context_line + 1]"
        , "first_context_line"
        ]
      , "FROM"
      , Q.aliasedSubquery inner_sql "foo"
      , "WHERE match_id = ?"
      ]

    match_details_fields = [
        "line_text"
      , "line_number"
      , "span_start"
      , "span_end"
      ]

    inner_sql = Q.qjoin [
        "SELECT"
      , Q.list $ [
          "COALESCE(content_lines, regexp_split_to_array(content, '\n')) AS array_output"
        , "GREATEST(0, matches_augmented.line_number - (matches_augmented.context_count + hidden_leading_line_count)) AS first_context_line"
        , "matches_augmented.line_number + matches_augmented.context_count AS last_context_line"
        , "matches_augmented.id AS match_id"
        ] ++ match_details_fields
      , "FROM log_metadata"
      , "JOIN"
      , "(SELECT * FROM matches, myconstants) matches_augmented"
      , "ON matches_augmented.build_step = log_metadata.step"
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
      , "WHERE step = ?"
      , "LIMIT 1"
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
  , "ON build_failure_causes.vcs_revision = ordered_master_commits.sha1"
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
      , "ORDER BY week DESC"
      , "LIMIT ?"
      , "OFFSET 1"
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

      MyUtils.guardE sanity_check_is_failure_count_equal
        "Sanity check failed: sanity_check_is_failure_count_equal"

      MyUtils.guardE sanity_check_total_is_successes_plus_failures
        "Sanity check failed: sanity_check_total_is_successes_plus_failures"

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
      , Q.list [
          "month"
        , "distinct_breakages"
        , "avoidable_count"
        ]
      , "FROM code_breakage_monthly_aggregation"
      , "ORDER BY month DESC"
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
      , "LIMIT ?"
      , "OFFSET 1"
      ]


getLatestKnownMasterCommit :: Connection -> IO (Maybe Text)
getLatestKnownMasterCommit conn = do
  rows <- query_ conn sql
  return $ Safe.headMay $ map (\(Only x) -> x) rows
  where
    sql = Q.qjoin [
        "SELECT sha1 FROM ordered_master_commits"
      , "ORDER BY id DESC"
      , "LIMIT 1"
      ]


isMasterCommit :: Builds.RawCommit -> DbIO Bool
isMasterCommit (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ do
    [Only exists] <- query conn master_commit_retrieval_sql $ Only sha1
    return exists
  where
    master_commit_retrieval_sql = Q.qjoin [
        "SELECT EXISTS"
      , Q.parens "SELECT * FROM ordered_master_commits WHERE sha1 = ?"
      ]


getAllMasterCommits :: Connection -> IO (Set Builds.RawCommit)
getAllMasterCommits conn = do
  master_commit_rows <- query_ conn master_commit_retrieval_sql
  return $ Set.fromList $ map (\(Only x) -> x) master_commit_rows
  where
    master_commit_retrieval_sql = "SELECT sha1 FROM ordered_master_commits"


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
    sql = "SELECT id FROM ordered_master_commits WHERE sha1 = ?"



data JobSpanWithSuccessStats = JobSpanWithSuccessStats {
    _job_name          :: Text
  , _total_built_count :: Int
  , _succeeded_count   :: Int
  } deriving (Generic, FromRow)

instance ToJSON JobSpanWithSuccessStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


knownBreakageAffectedJobs ::
     Int
  -> DbIO [DbHelpers.WithAuthorship JobSpanWithSuccessStats]
knownBreakageAffectedJobs cause_id = do
  conn <- ask
  liftIO $ query conn sql $ Only cause_id
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "reporter"
        , "reported_at"
        , "code_breakage_affected_jobs.job"
        , "total_built_count"
        , "succeeded_count"
        ]
      , "FROM code_breakage_affected_jobs"
      , "JOIN code_breakage_job_failure_counts"
      , "ON"
      , Q.qconjunction [
          "code_breakage_job_failure_counts.cause_id = code_breakage_affected_jobs.cause"
        , "code_breakage_job_failure_counts.job = code_breakage_affected_jobs.job"
        ]
      , "WHERE code_breakage_affected_jobs.cause = ?"
      , "ORDER BY code_breakage_affected_jobs.job ASC"
      ]


-- | Compare to: getSpanningBreakages
--
-- This query is only valid after the PR commit ancestor in the master branch
-- is cached in the database.
--
-- It does not need to know the master commit (merge base).
getInferredSpanningBrokenJobsBetter ::
     Builds.RawCommit -- ^ branch commit
  -> DbIO [SqlReadTypes.UpstreamBrokenJob]
getInferredSpanningBrokenJobsBetter (Builds.RawCommit branch_sha1) = do
  conn <- ask
  liftIO $ do
    D.debugList ["SQL:", show sql, "PARMS:", show branch_sha1]
    query conn sql $ Only branch_sha1
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "open_date"
        , "closed_date"
        , "open_sha1"
        , "closed_sha1"
        , "universal_build"
        , "provider"
        , "provider_build_num"
        , "span_length"
        ]
      , "FROM downstream_build_failures_from_upstream_inferred_breakages"
      , "WHERE branch_commit = ?"
      ]


-- | This only works for commits from the master branch.
-- Commits from other branches must use StatusUpdate.findKnownBuildBreakages
--
-- | Compare to: getInferredSpanningBrokenJobs
getSpanningBreakages ::
     Connection
  -> Builds.RawCommit
  -> IO (Either Text [DbHelpers.WithId CodeBreakage])
getSpanningBreakages conn sha1 =

  runExceptT $ do
    target_commit_index <- ExceptT $ getMasterCommitIndex conn sha1
    let parms = (target_commit_index, target_commit_index)
    rows <- liftIO $ query conn sql parms
    return $ map f rows

  where
    f (sha1, description, cause_id, jobs) = DbHelpers.WithId cause_id $
      CodeBreakage (Builds.RawCommit sha1) description $ Set.fromList $ fromPGArray jobs

    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "code_breakage_cause.sha1"
        , "code_breakage_cause.description"
        , "cause_id"
        , "COALESCE(jobs, '{}') AS jobs"
        ]
      , "FROM"
      , Q.aliasedSubquery inner_q "foo"
      , "JOIN code_breakage_cause"
      , "ON foo.cause_id = code_breakage_cause.id;"
      ]

    inner_q = Q.qjoin [
        "SELECT"
      , Q.list [
          "code_breakage_spans.cause_id"
        , "ARRAY_AGG(code_breakage_affected_jobs.job) AS jobs"
        ]
      , "FROM code_breakage_spans"
      , "LEFT JOIN code_breakage_affected_jobs"
      , "ON code_breakage_affected_jobs.cause = code_breakage_spans.cause_id"
      , "WHERE"
      , Q.qconjunction [
          "cause_commit_index <= ?"
        , Q.parens "resolved_commit_index IS NULL OR ? < resolved_commit_index"
        ]
      , "GROUP BY code_breakage_spans.cause_id"
      ]


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
  , "ORDER BY"
  , Q.list [
      "pattern_count DESC"
    , "build_matches DESC"
    ]
  ]


data MasterCommitAndSourcePr = MasterCommitAndSourcePr {
    _sha1      :: Builds.RawCommit
  , _pr_number :: Builds.PullRequestNumber
  } deriving (Generic, FromRow)

instance ToJSON MasterCommitAndSourcePr where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getPullRequestsWithMissingHeads :: DbIO [Builds.PullRequestNumber]
getPullRequestsWithMissingHeads = do
  xs <- runQuery sql
  return $ map (\(Only x) -> Builds.PullRequestNumber x) xs
  where
    sql = Q.qjoin [
        "SELECT"
      , "github_pr_number"
      , "FROM pull_requests_with_missing_heads"
      ]


getPullRequestsByCurrentHead ::
     Builds.RawCommit
  -> DbIO [Builds.PullRequestNumber]
getPullRequestsByCurrentHead (Builds.RawCommit commit_sha1) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only commit_sha1
    return $ map (\(Only x) -> Builds.PullRequestNumber x) xs
  where
    sql = Q.qjoin [
        "SELECT"
      , "pr_number"
      , "FROM pr_current_heads"
      , "WHERE head_sha1 = ?"
      ]


getAllMasterCommitPullRequests :: DbIO [MasterCommitAndSourcePr]
getAllMasterCommitPullRequests = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "sha1"
    , "github_pr_number"
    ]
  , "FROM master_ordered_commits_with_metadata"
  , "WHERE github_pr_number IS NOT NULL"
  , "ORDER BY id DESC"
  ]


-- | Gets Pull Request numbers of the commits that have been
-- implicated in master branch breakages
getImplicatedMasterCommitPullRequests :: DbIO [MasterCommitAndSourcePr]
getImplicatedMasterCommitPullRequests = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "cause_sha1"
    , "github_pr_number"
    ]
  , "FROM known_breakage_summaries_sans_impact"
  , "WHERE github_pr_number IS NOT NULL"
  , "ORDER BY cause_commit_index DESC"
  ]


apiAutocompleteTags :: Text -> DbIO [Text]
apiAutocompleteTags = listFlat1X $ Q.qjoin [
    "SELECT tag FROM"
  , Q.aliasedSubquery inner_sql "foo"
  , "WHERE tag ILIKE CONCAT(?,'%')"
  ]
  where
    inner_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "tag"
        , "COUNT(*) AS freq"
        ]
      , "FROM pattern_tags"
      , "GROUP BY tag"
      , "ORDER BY"
      , Q.list [
          "freq DESC"
        , "tag ASC"
        ]
      ]


apiAutocompleteSteps :: Text -> DbIO [Text]
apiAutocompleteSteps = listFlat1X $ Q.qjoin [
    "SELECT name FROM"
  , Q.aliasedSubquery inner_sql "foo"
  , "WHERE name ILIKE CONCAT(?,'%')"
  ]
  where
    inner_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "name"
        , "COUNT(*) AS freq"
        ]
      , "FROM build_steps"
      , "WHERE name IS NOT NULL"
      , "GROUP BY name"
      , "ORDER BY"
      , Q.list [
          "freq DESC"
        , "name ASC"
        ]
      ]


apiListSteps :: DbIO [Text]
apiListSteps = listFlat $ Q.qjoin [
    "SELECT name FROM build_steps"
  , "WHERE name IS NOT NULL"
  , "GROUP BY name"
  , "ORDER BY"
  , Q.list [
      "COUNT(*) DESC"
    , "name ASC"
    ]
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
  } deriving (Show, Generic, FromRow)


getFailedCircleCIJobNames ::
     Builds.RawCommit
  -> DbIO (Either LT.Text [Text])
getFailedCircleCIJobNames (Builds.RawCommit sha1) = do
  conn <- ask

  liftIO $ Right . map (\(Only x) -> x) <$> query conn sql query_parms
  where
    query_parms = (sha1, SqlReadTypes.circleCIProviderIndex)

    sql = Q.qjoin [
        "SELECT job_name"
      , "FROM global_builds"
      , "WHERE"
      , Q.qconjunction [
          "vcs_revision = ?"
        , "provider = ?"
        , "NOT succeeded"
        ]
      ]


countCircleCIFailures ::
     Builds.RawCommit
  -> DbIO (Either LT.Text Int)
countCircleCIFailures (Builds.RawCommit sha1) = do
  conn <- ask

  liftIO $ maybeToEither err . Safe.headMay . map (\(Only x) -> x) <$> query conn sql query_parms
  where
    query_parms = (sha1, SqlReadTypes.circleCIProviderIndex)

    err = LT.unwords [
        "No match for commit"
      , LT.fromStrict sha1
      ]

    sql = Q.qjoin [
        "SELECT COUNT(*)"
      , "FROM global_builds"
      , "WHERE"
      , Q.qconjunction [
          "vcs_revision = ?"
        , "provider = ?"
        , "NOT succeeded"
        ]
      ]


genBestBuildMatchQuery ::
     [Query]
  -> [Query]
  -> Query
genBestBuildMatchQuery fields_to_fetch sql_where_conditions =
  sql
  where
    -- TODO FIXME
    -- This is a copy of the logic from multiple nested views so that
    -- a query for a single git revision is optimized.
    -- Beware especially of divergence of the match ranking logic (e.g. on "specificity"),
    -- if the logic is updated in the VIEW definition on the database side.
    --
    -- See Github Issue #52
    sql = Q.qjoin [
        "SELECT"
      , Q.list fields_to_fetch
      , "FROM"
      , Q.aliasedSubquery best_match_subquery_sql "best_pattern_match_for_builds"
      , "JOIN matches ON matches.id = best_pattern_match_for_builds.match_id"
      , "JOIN log_metadata ON log_metadata.step = best_pattern_match_for_builds.step_id"
      , "JOIN global_builds ON global_builds.global_build_num = best_pattern_match_for_builds.universal_build"
      , "JOIN build_steps ON build_steps.universal_build = best_pattern_match_for_builds.universal_build"
      , "JOIN ci_providers ON ci_providers.id = global_builds.provider"
      ]

    best_match_subquery_sql = genBestMatchesSubquery sql_where_conditions


genBestMatchesSubquery :: [Query] -> Query
genBestMatchesSubquery sql_where_conditions = Q.qjoin [
    "SELECT DISTINCT ON"
  , Q.parens "inner_matches_for_build.universal_build"
  , Q.list [
      "build_num AS build"
    , "pattern AS pattern_id"
    , "expression"
    , "regex"
    , "is_retired"
    , "specificity"
    , "universal_build"
    , "match_id"
    , "step_id"
    , "vcs_revision"
    , "patterns.is_network"
    , "patterns.is_flaky"
    , "is_promoted"
    ]
  , "FROM"
  , Q.aliasedSubquery build_matches_subquery "inner_matches_for_build"
  , "JOIN patterns_rich AS patterns"
  , "ON inner_matches_for_build.pattern = patterns.id"
  , "ORDER BY"
    -- XXX Must keep these sort criteria in sync with the "best_pattern_match_for_builds" view definition
  , Q.list [
      "inner_matches_for_build.universal_build"
    , "inner_matches_for_build.is_promoted DESC"
    , "patterns.specificity DESC"
    , "inner_matches_for_build.line_number DESC"
    , "patterns.is_retired"
    , "patterns.regex"
    , "patterns.id DESC"
    , "inner_matches_for_build.match_id DESC"
    ]
  ]

  where
    build_matches_subquery = genAllBuildMatchesSubquery sql_where_conditions


genAllBuildMatchesSubquery :: [Query] -> Query
genAllBuildMatchesSubquery sql_where_conditions = Q.qjoin [
      "SELECT DISTINCT ON"
    , Q.parens $ Q.list [
        "step_id"
      , "pattern"
      , "line_number"
      ]
    , "*"
    , "FROM"
    , Q.aliasedSubquery filtered_matches_subquery "filtered_matches"
    , "ORDER BY"
    , Q.list [
        "step_id"
      , "pattern"
      , "line_number"
      , "is_promoted DESC"
      , "match_id DESC"
      ]
    ]
  where
  filtered_matches_subquery = Q.parens $ Q.qjoin [
      "SELECT"
    , Q.list [
        "matches.pattern"
      , "builds_join_steps.build_num"
      , "builds_join_steps.step_name"
      , "builds_join_steps.universal_build"
      , "matches.id AS match_id"
      , "matches.build_step AS step_id"
      , "matches.line_number"
      , "matches.line_text"
      , "matches.span_start"
      , "matches.span_end"
      , "builds_join_steps.vcs_revision"
      , "match_failure_elaborations.match IS NOT NULL AS is_promoted"
      ]
    , "FROM matches"
    , "JOIN builds_join_steps"
    , "ON matches.build_step = builds_join_steps.step_id"
    , "LEFT JOIN match_failure_elaborations"
    , "ON match_failure_elaborations.match = matches.id"
    , "WHERE"
    , Q.qconjunction sql_where_conditions
    ]


data RevisionBuildsWithTimeouts = RevisionBuildsWithTimeouts {
    timed_out_builds     :: [UnmatchedBuilds.UnmatchedBuild]
  , non_timed_out_builds :: [SqlReadTypes.StandardCommitBuildWrapper]
  } deriving (Generic, ToJSON)


-- | For commit-details page in web frontend and
-- for preparing PR-comment-posting data in postCommitSummaryStatus.
getRevisionBuilds ::
     GitRev.GitSha1
  -> DbIO (Either Text (DbHelpers.BenchmarkedResponse Float RevisionBuildsWithTimeouts))
getRevisionBuilds git_revision = do
  conn <- ask

  either_timed_out_builds <- apiTimeoutCommitBuilds $
    Builds.RawCommit $ GitRev.sha1 git_revision

  liftIO $ runExceptT $ do

    (timing, my_non_timed_out_builds) <- liftIO $ D.timeThisFloat $
      query conn sql sql_parms

    timed_out_builds <- except either_timed_out_builds

    return $ DbHelpers.BenchmarkedResponse timing $
      RevisionBuildsWithTimeouts
        timed_out_builds
        my_non_timed_out_builds

  where
    git_revision_text = GitRev.sha1 git_revision

    -- Note that we're passing the git revision as two separate query parameters:
    sql_parms = (SqlReadTypes.pullRequestCommentsLogContextLineCount, 0 :: Int, git_revision_text, git_revision_text)

    base_sql = genBestBuildMatchQuery fields_to_fetch inner_sql_where_conditions

    status_events_inner_sql = Q.qjoin [
        "SELECT *"
      , "FROM github_status_events_state_counts"
      , "WHERE"
        -- XXX We need this "redundant" sha1 query parm to prune (speed up) the inner query
      , "sha1 = ?"
      ]

    sql = Q.qjoin [
        "WITH myconstants"
      , "(context_count, hidden_leading_line_count) as (values (?, ?))"      , base_sql
      , "LEFT JOIN"
      , Q.aliasedSubquery status_events_inner_sql "github_status_events_state_counts"
      , "ON"
      , Q.qconjunction [
          "github_status_events_state_counts.sha1 = global_builds.vcs_revision"
        , "github_status_events_state_counts.job_name = global_builds.job_name"
        ]
      , "LEFT JOIN rebuild_trigger_event_counts"
      , "ON global_builds.global_build_num = rebuild_trigger_event_counts.universal_build"
      , "LEFT JOIN"
      , Q.aliasedSubquery outer_match_excerpt_sql "outer_match_excerpt"
      , "ON outer_match_excerpt.excerpt_match_id = match_id"
      ]


    -- XXX This query logic is duplicated in readLogSubset
    outer_match_excerpt_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "match_excerpts.array_output[match_excerpts.first_context_line_number + 1:match_excerpts.last_context_line_number + 1] AS excerpt_lines"
        , "match_excerpts.first_context_line_number"
        , "match_excerpts.match_id AS excerpt_match_id"
        ]
      , "FROM"
      , Q.aliasedSubquery inner_match_excerpt_sql "match_excerpts"
      ]


    inner_match_excerpt_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "COALESCE(content_lines, regexp_split_to_array(content, '\n')) AS array_output"
        , "GREATEST(0, matches_augmented.line_number - (matches_augmented.context_count + hidden_leading_line_count)) AS first_context_line_number"
        , "matches_augmented.line_number + matches_augmented.context_count AS last_context_line_number"
        , "matches_augmented.id AS match_id"
        ]
      , "FROM log_metadata"
      , "JOIN"
      , "(SELECT * FROM matches, myconstants) matches_augmented"
      , "ON matches_augmented.build_step = log_metadata.step"
      ]


    inner_sql_where_conditions = ["vcs_revision = ?"]

    fields_to_fetch = commit_build_fields ++ supplemental_fields

    commit_build_fields = [
        "build_steps.name AS step_name"
      , "match_id"
      , "build"
      , "global_builds.vcs_revision"
      , "queued_at"
      , "global_builds.job_name"
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
      , "FALSE as is_timeout"  -- TODO FIXME use the real value
      , "is_flaky"
      ]

    supplemental_fields = [
        Q.coalesce "github_status_events_state_counts.is_empirically_determined_flaky" "FALSE" "is_empirically_determined_flaky"
      , Q.coalesce "github_status_events_state_counts.has_completed_rerun" "FALSE" "has_completed_rerun"
      , "rebuild_trigger_event_counts.universal_build IS NOT NULL AS has_triggered_rebuild"
      , Q.coalesce "github_status_events_state_counts.failure_count" "0" "failure_count"
      , "outer_match_excerpt.excerpt_lines"
      , "outer_match_excerpt.first_context_line_number"
      ]


-- | Returns results in descending order of commit ID
getMasterCommits ::
     Pagination.ParentOffsetMode
  -> DbIO (Either Text (DbHelpers.InclusiveNumericBounds Int64, [BuildResults.IndexedRichCommit]))
getMasterCommits parent_offset_mode = do
  conn <- ask
  liftIO $ case parent_offset_mode of
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

    sql_first_commit_id = Q.qjoin [
        "SELECT id"
      , "FROM ordered_master_commits"
      , "ORDER BY id DESC"
      , "LIMIT 1"
      , "OFFSET ?;"
      ]

    sql_associated_commit_id = Q.qjoin [
        "SELECT id"
      , "FROM ordered_master_commits"
      , "WHERE sha1 = ?;"
      ]

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
      , "LIMIT ?"
      ]

    sql_commit_id_bounds = Q.qjoin [
        commits_query_prefix
      , "WHERE"
      , Q.qconjunction [
          "id >= ?"
        , "id <= ?"
        ]
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
    "SELECT"
  , Q.list [
      "id"
    , "vcs_revision"
    , "only_longitudinal_breakages"
    , "only_lateral_breakages"
    , "both_breakages"
    ]
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
  , "ON master_detected_breakages_without_annotations.universal_build = builds_join_steps.universal_build"
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
    is_empirically_determined_flaky <- field

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
      is_empirically_determined_flaky
      is_known_broken
      detected_directional_breakages
      is_serially_isolated
      ubuild_obj


refreshCachedMasterGrid ::
     Text
  -> Bool -- ^ was triggered from frontend
  -> DbIO (Either Text ())
refreshCachedMasterGrid view_name is_from_frontend = do
  conn <- ask
  liftIO $ runExceptT $ do
    sql_query <- except either_query
    liftIO $ do
      D.debugList [
          "Refreshing view"
        , MyUtils.quote $ T.unpack view_name
        ]

      (execution_time, _) <- D.timeThisFloat $ execute_ conn sql_query

      execute conn insertion_sql (view_name, execution_time, trigger_source)

      D.debugStr "View refreshed."

  where
    insertion_sql = Q.qjoin [
        "INSERT INTO lambda_logging.materialized_view_refresh_events"
      , Q.insertionValues [
          "view_name"
        , "execution_duration_seconds"
        , "event_source"
        ]
      ]

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
      , "ORDER BY timestamp DESC"
      , "LIMIT 1"
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
      , "ORDER BY latest_queued_at DESC"
      , "LIMIT 1"
      ]


data PostedPRComment = PostedPRComment {
    _pr_number      :: Int
  , _comment_id     :: Int64
  , _revision_id    :: Int64
  , _sha1           :: Builds.RawCommit
  , _body           :: Text
  , _created_at     :: UTCTime
  , _updated_at     :: UTCTime
  , _revision_count :: Int
  } deriving (Generic, FromRow)


genPRPostedCommentQuery where_clauses = Q.qjoin [
    "SELECT"
  , Q.list [
      "pr_number"
    , "comment_id"
    , "id"
    , "sha1"
    , "body"
    , "created_at"
    , "updated_at"
    , "revision_count"
    ]
  , "FROM latest_created_pull_request_comment_revision"
  , "WHERE"
  , Q.qconjunction where_clauses
  , "LIMIT 1"
  ]


getPostedCommentForPR ::
     Builds.PullRequestNumber
  -> DbIO (Maybe PostedPRComment)
getPostedCommentForPR (Builds.PullRequestNumber pr_number) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only pr_number
    return $ Safe.headMay xs
  where
    sql = genPRPostedCommentQuery ["pr_number = ?"]


getPostedCommentForSha1 ::
     Builds.RawCommit
  -> DbIO (Maybe PostedPRComment)
getPostedCommentForSha1 (Builds.RawCommit commit_sha1) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only commit_sha1
    return $ Safe.headMay xs
  where
    sql = genPRPostedCommentQuery ["sha1 = ?"]


data MaterializedViewRefreshInfo = MaterializedViewRefreshInfo {
    _view_name              :: Text
  , _latest                 :: UTCTime
  , _average_execution_time :: Double
  , _event_count            :: Int
  , _latest_age_seconds     :: Double
  , _latest_duration        :: Double
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
          , "latest_duration"
          ]
      , "FROM lambda_logging.materialized_view_refresh_event_stats"
      , "ORDER BY latest_age"
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
      , "job_name"
      , "FROM job_schedule_discriminated_mview"
      , "WHERE inferred_scheduled"
      , "ORDER BY job_name"
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
      , "WHERE"
      , Q.qconjunction [
          "not_succeeded_required_job_count <= ?"
        , "failed_required_job_count <= ?"
        ]
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
      , "WHERE"
      , Q.qconjunction [
          "inserted_at > ?::timestamp - interval '? weeks'"
        , "commit_count_behind IS NOT NULL"
        ]
      , "ORDER BY inserted_at DESC"
      ]


getBreakageSpans ::
     DbHelpers.InclusiveNumericBounds Int64
  -> DbIO [BuildResults.JobFailureSpan]
getBreakageSpans commit_id_bounds = do
  conn <- ask
  liftIO $ query conn job_failure_spans_sql parms_tuple
  where
    bounds_tuple = DbHelpers.boundsAsTuple commit_id_bounds
    parms_tuple = (fst bounds_tuple, snd bounds_tuple, fst bounds_tuple, snd bounds_tuple)

    job_failure_spans_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "job_name"
        , "int8range(?, ?, '[]') * failure_commit_id_range AS commit_id_span"
        , "span_length"
        ]
      , "FROM master_job_failure_spans_conservative_mview"
      , "WHERE"
      , Q.qconjunction [
          "int8range(?, ?, '[]') && failure_commit_id_range"
        , "COALESCE(span_length > 1, TRUE)"
        ]
      , "ORDER BY commit_id_span DESC"
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

--    liftIO $ D.debugStr "Z A"
    (commits_list_time, (commit_id_bounds, master_commits)) <- D.timeThisFloat $
      ExceptT $ flip runReaderT conn $ getMasterCommits $ Pagination.offset_mode timeline_parms

    let commit_bounds_tuple = DbHelpers.boundsAsTuple commit_id_bounds

--    liftIO $ D.debugStr "Z B"
    (code_breakages_time, code_breakage_ranges) <- D.timeThisFloat $ liftIO $
      runReaderT (apiAnnotatedCodeBreakages commit_id_bounds) conn

--    liftIO $ D.debugStr "Z C"
    (job_failure_spans_time, job_failure_spans) <- D.timeThisFloat $
      liftIO $ runReaderT (getBreakageSpans commit_id_bounds) conn

--    liftIO $ D.debugStr "Z D"
    (reversion_spans_time, reversion_spans) <- D.timeThisFloat $
      liftIO $ query conn reversion_spans_sql commit_bounds_tuple

--    liftIO $ D.debugStr "Z E"
    (builds_list_time, completed_builds) <- D.timeThisFloat $
      liftIO $ query conn builds_list_sql commit_bounds_tuple

--    liftIO $ D.debugStr "Z F"
    (disjoint_statuses_time, disjoint_statuses) <- D.timeThisFloat $
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

        -- Even though we've fetched all these builds from the DB,
        -- we don't bother to send builds to the client that won't
        -- be displayed.
        filtered_builds_list = filter ((`Set.member` filtered_job_names) . Builds.job_name . BuildResults._build) completed_builds

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
      filtered_builds_list
      code_breakage_ranges
      disjoint_statuses
      job_failure_spans
      reversion_spans

  where
    suppress_scheduled_builds = Pagination.should_suppress_scheduled_builds $
      Pagination.column_filtering timeline_parms

    reversion_spans_sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "reverted_commit_id"
        , "reversion_commit_id"
        ]
      , "FROM master_commit_reversion_spans_mview"
      , "WHERE"
      , "int8range(?, ?, '[]') && reversion_span"
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
      , "WHERE"
      , "int8range(?, ?, '[]') @> ordered_master_commits.id::int8"
      ]


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
        , "COALESCE(queued_at, NOW()) AS queued_at" -- FIXME why is this sometimes null?
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
        , "is_empirically_determined_flaky"
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
      , "WHERE"
      , "int8range(?, ?, '[]') @> commit_index::int8"
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
    , Q.list [
        "github_pr_number"
      , "foreshadowed_by_pr_failures"
      , "start_date"
      , "end_date"
      ]
    , "FROM code_breakage_nonoverlapping_spans_dated"
    ]

  dirty_master_spans <- runQuery $ Q.qjoin [
      "SELECT"
    , Q.list [
        "group_index"
      , "breakage_start"
      , "breakage_end"
      ]
    , "FROM master_indiscriminate_failure_spans"
    , "ORDER BY breakage_start"
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


data GitHubCircleCIStatusEvent = GitHubCircleCIStatusEvent {
    _sha1         :: Builds.RawCommit
  , _created_at   :: UTCTime
  , _job_name     :: Text
  , _build_number :: Builds.BuildNumber
  , _state        :: Text
  } deriving (Generic, FromRow)

instance ToJSON GitHubCircleCIStatusEvent where
  toJSON = genericToJSON JsonUtils.dropUnderscore


apiGitHubNotificationsForBuild ::
     Text
  -> Builds.RawCommit
  -> DbIO [GitHubCircleCIStatusEvent]
apiGitHubNotificationsForBuild job_name (Builds.RawCommit sha1_text) = do
  conn <- ask
  liftIO $ query conn sql (job_name, sha1_text)
  where
  sql = Q.qjoin [
      "SELECT"
    , Q.list [
        "sha1"
      , "created_at"
      , "job_name_extracted"
      , "build_number_extracted"
      , "state"
      ]
    , "FROM github_status_events_circleci"
    , "WHERE"
    , Q.qconjunction [
        "job_name_extracted = ?"
      , "sha1 = ?"
      ]
    , "ORDER BY created_at DESC"
    ]


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
  , "WHERE build_count > 1"
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
  , "ORDER BY first_commit_id DESC"
  ]


apiListFailureModes :: DbIO [DbHelpers.WithId BuildResults.MasterFailureModeDetails]
apiListFailureModes = runQuery $ Q.qjoin [
    "SELECT"
  , Q.list [
      "id"
    , "label"
    , "revertible"
    ]
  , "FROM master_failure_modes"
  , "ORDER BY id"
  ]


annotatedCodeBreakagesFields = [
    "cause_id"
  , "cause_commit_index"
  , "cause_sha1"
  , "description"
  , "failure_mode_reporter"
  , "failure_mode_reported_at"
  , "failure_mode_id"
  , "cause_reporter"
  , "cause_reported_at"
  , "cause_jobs_array"
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
     DbHelpers.InclusiveNumericBounds Int64
  -> DbIO [BuildResults.BreakageSpan Text ()]
apiAnnotatedCodeBreakages commit_id_bounds = do
  conn <- ask
  liftIO $ query conn sql $ DbHelpers.boundsAsTuple commit_id_bounds
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list annotatedCodeBreakagesFields
      , "FROM known_breakage_summaries_sans_impact"
      , "WHERE"
      , "int8range(?, ?, '[]') && commit_index_span"
      , "ORDER BY cause_commit_index DESC"
      ]


sqlPrefixAnnotatedCodeBreakagesWithImpact = Q.qjoin [
    "SELECT"
  , Q.list $ annotatedCodeBreakagesFields ++ [
      "downstream_broken_commit_count"
    , "failed_downstream_build_count"
    , "github_pr_number"
    , "github_pr_head_commit"
    , "foreshadowed_broken_jobs_array"
    ]
  , "FROM known_breakage_summaries"
  ]


apiAnnotatedCodeBreakagesWithImpact ::
  DbIO [BuildResults.BreakageSpan Text BuildResults.BreakageImpactStats]
apiAnnotatedCodeBreakagesWithImpact = runQuery $ Q.qjoin [
    sqlPrefixAnnotatedCodeBreakagesWithImpact
  , "ORDER BY cause_commit_index DESC"
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
      , "WHERE cause_id = ?"
      ]


apiAnnotatedCodeBreakagesWithoutImpactSingle ::
     Int
  -> DbIO (Either Text (DbHelpers.BenchmarkedResponse Float (BuildResults.BreakageSpan Text ())))
apiAnnotatedCodeBreakagesWithoutImpactSingle cause_id = do
  conn <- ask
  liftIO $ do
    (timing, xs) <- D.timeThisFloat $ query conn sql $ Only cause_id
    return $ maybeToEither "Not found" $ DbHelpers.BenchmarkedResponse timing <$> Safe.headMay xs

  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list annotatedCodeBreakagesFields
      , "FROM known_breakage_summaries_sans_impact"
      , "WHERE cause_id = ?"
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
  , "ORDER BY distinct_breakage_count DESC"
  ]


apiBrokenCommitsWithoutMetadata :: DbIO [Builds.RawCommit]
apiBrokenCommitsWithoutMetadata = runQuery
  "SELECT vcs_revision FROM broken_commits_without_metadata"


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
      , "LIMIT 1"
      ]


checkHasTestResults ::
     Connection
  -> Builds.ProviderSurrogateId
  -> IO Bool
checkHasTestResults
  conn
  (Builds.ProviderSurrogateId provider_surrogate_id) = do

  [Only result] <- query conn sql $ Only provider_surrogate_id
  return result
  where
    sql = Q.qjoin [
        "SELECT EXISTS"
      , Q.parens $ Q.qjoin [
          "SELECT *"
        , "FROM circleci_test_reports"
        , "WHERE provider_build_surrogate_id = ?"
        , "LIMIT 1"
        ]
      ]


getProviderSurrogateIdFromUniversalBuild ::
     Builds.UniversalBuildId
  -> DbIO (Either Text Builds.ProviderSurrogateId)
getProviderSurrogateIdFromUniversalBuild (Builds.UniversalBuildId ubuild_id) = do
  conn <- ask
  liftIO $ do
    xs <- query conn sql $ Only ubuild_id
    return $ maybeToEither err_msg $ Safe.headMay $
      map (\(Only x) -> Builds.ProviderSurrogateId x) xs
  where
    err_msg = T.unwords [
        "build"
      , T.pack $ show ubuild_id
      , "not in database"
      ]

    sql = Q.qjoin [
        "SELECT"
      , "provider_build_surrogate"
      , "FROM universal_builds"
      , "WHERE id = ?"
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
    apply_pattern line_tuple = ScanUtils.convertMatchAnswerToMaybe $
      ScanUtils.applySinglePattern id line_tuple $ DbHelpers.WithId 0 new_pattern


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
      , "WHERE id = ?"
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
  , "ORDER BY most_recent DESC NULLS LAST"
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
        , "tags_array"
        , "steps_array"
        , "specificity"
        , "is_retired"
        , "lines_from_end"
        ]
      , "FROM patterns_augmented"
      , "ORDER BY id"
      ]

    f ( author
      , created
      , pattern_id
      , is_regex
      , expression
      , has_nondeterministic_values
      , description
      , tags
      , steps
      , specificity
      , is_retired
      , lines_from_end
      ) = DbHelpers.WithAuthorship author created $ wrapPattern
        pattern_id
        is_regex
        expression
        has_nondeterministic_values
        description
        (fromPGArray tags)
        (fromPGArray steps)
        specificity
        is_retired
        lines_from_end


data PatternInfoSpecifictySubset = PatternInfoSpecifictySubset {
    _specificity :: Int
  , _is_regex    :: Bool
  } deriving (Generic, FromRow)

instance ToJSON PatternInfoSpecifictySubset where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data AugmentedPatternOccurrence a = AugmentedPatternOccurrence {
    _occurrence   :: PatternOccurrence
  , _pattern_info :: a
  } deriving Generic

instance (ToJSON a) => ToJSON (AugmentedPatternOccurrence a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


instance (FromRow a) => FromRow (AugmentedPatternOccurrence a) where
  fromRow = AugmentedPatternOccurrence <$> fromRow <*> fromRow


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
  } deriving (Generic, FromRow)

instance ToJSON PatternOccurrence where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getBuildPatternMatches ::
     Builds.UniversalBuildId
  -> DbIO (DbHelpers.BenchmarkedResponse Float [MatchOccurrences.MatchOccurrencesForBuild])
getBuildPatternMatches (Builds.UniversalBuildId build_id) = do
  conn <- ask

  (timing, result) <- D.timeThisFloat $ liftIO $ query conn sql $ Only build_id
  return $ DbHelpers.BenchmarkedResponse timing result
  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "build_steps.name AS step_name"
        , "pattern"
        , "match_id"
        , "line_number"
        , "line_count"
        , "line_text"
        , "span_start"
        , "span_end"
        , "specificity"
        ]
      , "FROM"
      , Q.aliasedSubquery build_matches_subquery "all_build_matches"
      , "JOIN build_steps"
      , "ON all_build_matches.step_id = build_steps.id"
      , "JOIN log_metadata"
      , "ON log_metadata.step = build_steps.id"
      , "JOIN patterns"
      , "ON all_build_matches.pattern = patterns.id"
      , "ORDER BY"
        -- XXX This sort criteria list should be kept in sync with the "best match" view
      , Q.list [
          "is_promoted DESC"
        , "specificity DESC"
        , "pattern ASC"
        , "line_number ASC"
        ]
      ]

    sql_where_conditions = ["universal_build = ?"]
    build_matches_subquery = genAllBuildMatchesSubquery sql_where_conditions


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


-- | This is currently only used in one place: "getBestPatternMatches"
commonQueryPrefixPatternMatches :: Query
commonQueryPrefixPatternMatches = Q.qjoin [
    "SELECT"
  , Q.list [
      "build"
    , "pattern_id"
    , "match_id"
    , "vcs_revision"
    , "queued_at"
    , "job_name"
    , "branch"
    , "step_name"
    , "line_number"
    , "line_count"
    , "line_text"
    , "span_start"
    , "span_end"
    , "universal_build"
    ]
  , "FROM best_pattern_match_augmented_builds"
  , "WHERE pattern_id = ?"
  , "ORDER BY"
  , "queued_at DESC"
  ]


-- | Limit is arbitrary
getBestPatternMatches ::
     ScanPatterns.PatternId
  -> DbIO [PatternOccurrence]
getBestPatternMatches (ScanPatterns.PatternId pattern_id) = do
  conn <- ask
  liftIO $ query conn sql $ Only pattern_id

  where
    sql = Q.qjoin [
        commonQueryPrefixPatternMatches
      , "LIMIT 100"
      ]


-- | This should produce one or zero results.
-- We use a list instead of a Maybe so that
-- the javascript table renderer code can be reused
-- for multi-item lists.
getBestBuildMatch ::
     Builds.UniversalBuildId
  -> DbIO (DbHelpers.BenchmarkedResponse Float [AugmentedPatternOccurrence PatternInfoSpecifictySubset])
getBestBuildMatch (Builds.UniversalBuildId build_id) = do

  conn <- ask
  (timing, content) <- D.timeThisFloat $ liftIO $ query conn sql $ Only build_id

  return $ DbHelpers.BenchmarkedResponse timing content

  where
    fields_to_fetch = [
        "build"
      , "pattern_id"
      , "match_id"
      , "global_builds.vcs_revision"
      , "queued_at"
      , "job_name"
      , "branch"
      , "build_steps.name AS step_name"
      , "line_number"
      , "line_count"
      , "line_text"
      , "span_start"
      , "span_end"
      , "global_builds.global_build_num"
      , "specificity"
      , "regex"
      ]

    sql_where_conditions = ["universal_build = ?"]
    sql = genBestBuildMatchQuery fields_to_fetch sql_where_conditions


logContextFunc ::
     Int -- ^ Hidden context linecount
  -> MatchOccurrences.MatchId
  -> Int
  -> DbIO (Either Text CommitBuilds.LogContext)
logContextFunc
    hidden_context_linecount
    mid
    context_linecount = do

  conn <- ask
  liftIO $ runExceptT $ do

    (match_info, excerpt_lines_and_start_number) <- ExceptT $ runReaderT
      (readLogSubset mid context_linecount hidden_context_linecount)
      conn

    return $ CommitBuilds.LogContext match_info $
      CommitBuilds.toNumberedLineTuples excerpt_lines_and_start_number


getPatternMatches ::
     ScanPatterns.PatternId
  -> DbIO [PatternOccurrence]
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


-- | Currently this is only used by the "getPatternMatches" function above,
-- which uses a lot less info than this fetches.
--
-- Limit is arbitrary.
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
      , "WHERE pattern = ?"
      , "ORDER BY"
      , "queued_at DESC"
      , "LIMIT 100"
      ]