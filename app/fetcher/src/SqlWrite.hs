{-# LANGUAGE OverloadedStrings #-}

module SqlWrite where

import           Control.Applicative               ((<|>))
import           Control.Exception                 (throwIO)
import           Control.Monad                     (unless)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (ExceptT (ExceptT), except,
                                                    runExceptT)
import           Control.Monad.Trans.Reader        (ask, runReaderT)
import           Data.Bifunctor                    (first)
import qualified Data.ByteString.Char8             as BS
import           Data.Either                       (partitionEithers)
import           Data.Either.Utils                 (maybeToEither)
import           Data.Foldable                     (for_)
import           Data.List.Ordered                 (nubSort)
import qualified Data.Maybe                        as Maybe
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Time                         (UTCTime)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    formatTime,
                                                    rfc822DateFormat)
import           Data.Traversable                  (for)
import           Data.Tuple                        (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Errors
import           Database.PostgreSQL.Simple.Types  (PGArray (PGArray),
                                                    fromPGArray)
import           GHC.Int                           (Int64)
import qualified Network.OAuth.OAuth2              as OAuth2
import qualified Safe

import qualified ApiPost
import qualified AuthStages
import qualified Breakages
import qualified Builds
import qualified Commits
import qualified Constants
import qualified DbHelpers
import qualified DebugUtils                        as D
import qualified GadgitFetch
import qualified GithubApiFetch
import qualified GitHubRecords
import qualified MatchOccurrences
import qualified MergeBase
import qualified QueryUtils                        as Q
import qualified ScanPatterns
import qualified ScanRecords
import qualified SqlRead
import qualified Webhooks


sqlInsertUniversalBuild :: Query
sqlInsertUniversalBuild = Q.qjoin [
    "INSERT INTO universal_builds"
  , Q.insertionValues [
      "provider"
    , "build_number"
    , "build_namespace"
    , "succeeded"
    , "commit_sha1"
    ]
  , "ON CONFLICT"
  , "ON CONSTRAINT universal_builds_build_number_build_namespace_provider_key"
  , "DO UPDATE SET build_number = excluded.build_number"
  , "RETURNING id;"
  ]


storeCommitMetadata ::
     Connection
  -> [Commits.CommitMetadata]
  -> IO (Either Text Int64)
storeCommitMetadata conn commit_list =

  catchViolation catcher $ do
    count <- executeMany conn insertion_sql $ map f commit_list
    return $ Right count

  where
    f (Commits.CommitMetadata (Builds.RawCommit sha1) message tree_sha1 author_name author_email author_date committer_name committer_email committer_date) = (sha1, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date)

    insertion_sql = Q.qjoin [
        "INSERT INTO commit_metadata"
      , Q.insertionValues [
          "sha1"
        , "message"
        , "tree_sha1"
        , "author_name"
        , "author_email"
        , "author_date"
        , "committer_name"
        , "committer_email"
        , "committer_date"
        ]
      ]

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


-- | Caches the result.
--
-- In general, it would be possible for the merge base to change
-- over time if the master branch is advanced to a more recent
-- ancestor (up to and including the HEAD) of the PR branch.
-- In practice, however, this will not happen in the Facebook-mirrored
-- repo configuration, as a novel commit is produced for every change
-- to the master branch.
--
-- Therefore, this cache of merge bases never needs to be invalidated.
findMasterAncestorWithPrecomputation ::
     Maybe (Set Builds.RawCommit)
     -- ^ all master commits; passing this in can save time in a loop
  -> Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> CacheMergeBaseOption
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
findMasterAncestorWithPrecomputation
  maybe_all_master_commits
  conn
  access_token
  owner_and_repo
  merge_base_cache_option
  sha1@(Builds.RawCommit unwrapped_sha1) =

  -- This is another optimization:
  -- if we have pre-computed the master commit list, we
  -- may get lucky and find that the requested commit is a member, in which
  -- case we can avoid a database lookup to the merge-base cache.
  if Set.member sha1 $ Maybe.fromMaybe Set.empty maybe_all_master_commits
  then do
    D.debugList [
        "Bypassed cache since"
      , T.unpack unwrapped_sha1
      , "is a master commit!"
      ]

    return $ Right sha1
  else do

    known_merge_base_rows <- query conn cached_merge_bases_sql $
      Only unwrapped_sha1

    let maybe_cached_merge_base = Safe.headMay $
          map (\(Only x) -> x) known_merge_base_rows

    case maybe_cached_merge_base of
      Just cached_merge_base -> do
        D.debugList [
            "Retrieved merge base of"
          , show sha1
          , "from cache as"
          , show cached_merge_base
          ]

        return $ Right cached_merge_base
      Nothing -> do

        -- Optimization: we can make use of pre-computed commit set
        -- if we're processing several commit ancestors.  Otherwise,
        -- single ancestor retrievals may not need the master commit list
        -- if the answer is in the cache already.
        known_commit_set <- case maybe_all_master_commits of
          Nothing -> SqlRead.getAllMasterCommits conn
          Just x  -> return x

        runExceptT $ do

          (merge_base_commit@(Builds.RawCommit unwrapped_merge_base), distance) <- ExceptT $ first TL.toStrict <$> GithubApiFetch.findAncestor
            access_token
            owner_and_repo
            sha1
            known_commit_set

          -- Distance 0 means it was a member of the master branch.
          unless (distance == 0 || merge_base_cache_option == NoCache) $ liftIO $ do
            execute conn merge_base_insertion_sql (unwrapped_sha1, unwrapped_merge_base, distance)
            D.debugList [
                "Stored merge base of"
              , T.unpack unwrapped_sha1
              , "to cache as"
              , T.unpack unwrapped_merge_base
              , "with distance"
              , show distance
              ]

          return merge_base_commit

  where
    cached_merge_bases_sql = Q.qjoin [
        "SELECT master_commit"
      , "FROM cached_master_merge_base"
      , "WHERE branch_commit = ?;"
      ]

    merge_base_insertion_sql = Q.qjoin [
        "INSERT INTO cached_master_merge_base"
      , Q.insertionValues [
          "branch_commit"
        , "master_commit"
        , "distance"
        ]
      ]


-- | for in case we want to use the
-- "findMasterAncestor" function without side effects.
data CacheMergeBaseOption = StoreToCache | NoCache
  deriving Eq


findMasterAncestor ::
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> CacheMergeBaseOption
  -> Builds.RawCommit
  -> IO (Either Text Builds.RawCommit)
findMasterAncestor = findMasterAncestorWithPrecomputation Nothing


-- | Also populates pull_request_heads table
updateMergedPullRequestHeadCommits ::
     Connection
  -> IO (Either T.Text [(Builds.PullRequestNumber, Builds.RawCommit)])
updateMergedPullRequestHeadCommits conn = do
  unmatched_pr_numbers <- runReaderT SqlRead.getPullRequestsWithMissingHeads conn

  runExceptT $ do
    pr_associations <- ExceptT $ first T.pack <$> GadgitFetch.getPullRequestHeadCommitsBulk unmatched_pr_numbers

    let pr_head_eithers = map (first T.pack . sequenceA) pr_associations
        (unretrieved_pr_heads, retrieved_pr_heads) = partitionEithers pr_head_eithers

    liftIO $ D.debugList [
        "Retrieved"
      , show $ length retrieved_pr_heads
      , "out of"
      , show $ length unmatched_pr_numbers
      , "unassociated PR HEAD commits."
      , show $ length unretrieved_pr_heads
      , "were not available."
      ]

    let deduped_insertion_records = nubSort $ map (\(Builds.PullRequestNumber x, Builds.RawCommit y) -> (x, y)) retrieved_pr_heads
    inserted_count <- liftIO $ executeMany conn insertion_sql deduped_insertion_records

    liftIO $ D.debugList [
        "Inserted"
      , show inserted_count
      , "new rows into pull_request_heads table"
      ]

    return retrieved_pr_heads
  where
    insertion_sql = Q.qjoin [
        "INSERT INTO pull_request_heads"
      , Q.insertionValues [
          "pr_number"
        , "head_sha1"
        ]
      , "ON CONFLICT"
      , "ON CONSTRAINT pull_request_heads_pr_number_head_sha1_key"
      , "DO UPDATE SET timestamp = NOW();"
      ]


storeCachedMergeBases ::
     Connection
  -> [MergeBase.CommitMergeBase]
  -> IO (Either Text Int64)
storeCachedMergeBases conn merge_base_records =
  catchViolation catcher $ do
    count <- executeMany conn insertion_sql $ map f merge_base_records
    return $ Right count

  where
    f (MergeBase.CommitMergeBase (Builds.RawCommit branch_commit) (Builds.RawCommit master_commit) distance) = (branch_commit, master_commit, distance)

    insertion_sql = Q.qjoin [
        "INSERT INTO cached_master_merge_base"
      , Q.insertionValues [
          "branch_commit"
        , "master_commit"
        , "distance"
        ]
      ]

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


-- | If we went through too many remote commits trying to find the
-- locally-stored "tip" of the master branch, it may be that the
-- local tip is incorrect; the remote branch may have been rebased
-- (this actually has happened before).
-- In that case, we can verify by finding the merge base of the
-- remote "master" branch with our local master branch.
diagnoseCommitsFetchFailure
    conn
    access_token
    owned_repo
    either_result =

  case either_result of
    Left (x, y) -> case x of
      GithubApiFetch.TooManyCommits retrieved_commits -> do
        either_ancestor <- findMasterAncestor
          conn
          access_token
          owned_repo
          NoCache
          (Builds.RawCommit $ GitHubRecords.extractCommitSha $ head retrieved_commits)

        return $ Left $ TL.pack $ unwords $ case either_ancestor of
          Left x -> [
              "Error while diagnosing master commits update:"
            , T.unpack x
            ]

          Right x -> [
              "Last accurate local master commit was:"
            , show x
            , "The administrator should purge all later commits"
            , "from the cache!"
            ]

      _ -> return $ Left y
    Right x -> return $ Right x


populateLatestMasterCommits ::
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> IO (Either Text (Int64, Int64))
populateLatestMasterCommits conn access_token owned_repo = do

  D.debugStr "Populating latest master commits..."

  maybe_latest_known_commit <- SqlRead.getLatestKnownMasterCommit conn

  D.debugList [
      "Latest known master commit:"
    , show maybe_latest_known_commit
    ]

  runExceptT $ do

    -- The admin must manually populate the first several thousand commits,
    -- as these would be inefficient to fetch from the GitHub API.
    latest_known_commit <- except $ maybeToEither "Database has no commits" maybe_latest_known_commit

    fetched_commits_newest_first <- ExceptT $ fmap (first TL.toStrict) $ do
      either_result <- GithubApiFetch.getCommitsNewestFirst
        access_token
        owned_repo
        Constants.masterName
        latest_known_commit

      diagnoseCommitsFetchFailure
        conn
        access_token
        owned_repo
        either_result


    let fetched_commits_oldest_first = reverse fetched_commits_newest_first

    commit_insertion_count <- ExceptT $ storeMasterCommits conn $
      map GitHubRecords.extractCommitSha fetched_commits_oldest_first

    liftIO $ D.debugList [
        "Inserted "
      , show commit_insertion_count
      , "commits"
      ]

    metadata_insertion_count <- ExceptT $ storeCommitMetadata conn $ map Commits.fromGithubRecord fetched_commits_oldest_first

    return (commit_insertion_count, metadata_insertion_count)


storeMasterCommits ::
     Connection
  -> [Text]
  -> IO (Either Text Int64)
storeMasterCommits conn commit_list =

  catchViolation catcher $ do
    count <- executeMany conn insertion_sql $ map Only commit_list
    return $ Right count

  where
    insertion_sql = Q.qjoin [
        "INSERT INTO ordered_master_commits"
      , Q.insertionValues ["sha1"]
      ]

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


-- | TODO we may only need the multiple-row version of this
insertSingleUniversalBuild ::
     Connection
  -> Builds.UniversalBuild
  -> IO (DbHelpers.WithId Builds.UniversalBuild)
insertSingleUniversalBuild
    conn
    uni_build = do

  [Only new_id] <- query conn sqlInsertUniversalBuild tup
  return $ DbHelpers.WithId new_id uni_build

  where
    tup = (provider_id, provider_buildnum, build_namespace, succeeded, sha1)
    Builds.UniversalBuild (Builds.NewBuildNumber provider_buildnum) provider_id build_namespace succeeded (Builds.RawCommit sha1) = uni_build


-- | We handle de-duplication of provider-build records
-- via the ON CONFLICT clause in the INSERT statement.
-- When a record already exists with a given
-- (provider_build_number, build_namespace, provider_key)
-- tuple, the existing universal build ID is returned instead
-- of creating a new row.
--
-- However, note that we must not have two rows proposed for insertion
-- that conflict amongst each other, so we must
-- perform some client-side deduplication first (in this case, with nubSort).
--
-- Duplicates may arise due to the way pagination of the CircleCI builds list
-- works. Say that 200 builds are to be fetched, with 100 per page. If, while
-- processing the first page, a new build is completed on CircleCI, then
-- 100th build from the first page will get bumped to position 101, and fetched
-- again as the first build of the second page.
--
-- See https://pganalyze.com/docs/log-insights/app-errors/U126
--
-- TODO for now, this function is only called from the standalone scanner application.
storeCircleCiBuildsList ::
     Connection
  -> UTCTime -- ^ fetch initiation time
  -> Text -- ^ branch name
  -> Maybe Int64 -- ^ EB worker event ID
  -> [(Builds.Build, Bool)]
  -> IO Int64
storeCircleCiBuildsList
    conn
    fetch_initiation_timestamp
    branch_name
    maybe_eb_worker_event_id
    builds_list_with_possible_duplicates = do

  universal_build_insertion_output_rows <- returning
    conn
    sqlInsertUniversalBuild
    (map input_f universal_builds)

  let zipped_output1 = zipWith
        (\(Only row_id) ubuild -> DbHelpers.WithId row_id ubuild)
        universal_build_insertion_output_rows
        universal_builds

      zipped_output2 = zipWith
        (\(DbHelpers.WithId ubuild_id _ubuild) rbuild -> DbHelpers.WithTypedId (Builds.UniversalBuildId ubuild_id) rbuild)
        zipped_output1
        (map fst deduped_builds_list)

  let store_scan_record = storeScanRecord
        SqlRead.circleCIProviderIndex
        branch_name
        fetch_initiation_timestamp
        maybe_eb_worker_event_id

  ci_scan_id <- runReaderT store_scan_record conn

  storeBuildsList conn (Just ci_scan_id) zipped_output2

  where
    deduped_builds_list = nubSort builds_list_with_possible_duplicates

    mk_ubuild (b, succeeded) = Builds.UniversalBuild
      (Builds.build_id b)
      SqlRead.circleCIProviderIndex
      "" -- no build numbering namespace qualifier for CircleCI builds
      succeeded
      (Builds.vcs_revision b)

    universal_builds = map mk_ubuild deduped_builds_list

    input_f (Builds.UniversalBuild (Builds.NewBuildNumber provider_buildnum) provider_id build_namespace succeeded (Builds.RawCommit sha1)) = (provider_id, provider_buildnum, build_namespace, succeeded, sha1)


-- | This is idempotent; builds that are already present will not be overwritten
--
-- Need to make sure that legit values are not overwritten with NULL
storeBuildsList ::
     Connection
  -> Maybe Int64
  -> [DbHelpers.WithTypedId Builds.UniversalBuildId Builds.Build]
  -> IO Int64
storeBuildsList conn maybe_provider_scan_id builds_list = do

  D.debugList [
      "Inside storeBuildsList to update"
    , show $ length builds_list
    , "build entries"
    ]

  for_ builds_list $ \x -> do
    execute conn sql $ f x
    return ()

  D.debugList [
      "Finishing storeBuildsList."
    ]

  return $ fromIntegral $ length builds_list

  where
    f (DbHelpers.WithTypedId (Builds.UniversalBuildId universal_build_id) rbuild) =
      ( queued_at_string
      , jobname
      , branch
      , start_time
      , stop_time
      , maybe_provider_scan_id
      , universal_build_id
      )
      where
        queued_at_string = T.pack $ formatTime defaultTimeLocale rfc822DateFormat queuedat
        (Builds.NewBuild _ _ queuedat jobname branch start_time stop_time) = rbuild

    sql = Q.qjoin [
        "UPDATE universal_builds"
      , "SET"
      , Q.list [
          "x_queued_at = ?"
        , "x_job_name = ?"
        , "x_branch = ?"
        , "x_started_at = ?"
        , "x_finished_at = ?"
        , "x_ci_provider_scan = ?"
        ]
      , "WHERE"
      , "id = ?"
      ]

    {-
    sql = Q.qjoin [
        "INSERT INTO builds"
      , Q.insertionValues [
          "queued_at"
        , "job_name"
        , "branch"
        , "global_build_num"
        , "started_at"
        , "finished_at"
        , "ci_provider_scan"
        ]
      , "ON CONFLICT (global_build_num)"
      , "DO UPDATE"
      , "SET"
      , Q.list [
          "branch = COALESCE(builds.branch, EXCLUDED.branch)"
        , "queued_at = COALESCE(builds.queued_at, EXCLUDED.queued_at)"
        , "started_at = COALESCE(builds.started_at, EXCLUDED.started_at)"
        , "finished_at = COALESCE(builds.finished_at, EXCLUDED.finished_at)"
        ]
      ]
    -}


storeScanRecord ::
     Int64 -- ^ provider ID
  -> Text -- ^ branch filter
  -> UTCTime -- ^ scan initiation time
  -> Maybe Int64 -- ^ EB worker event ID
  -> SqlRead.DbIO Int64
storeScanRecord provider_id branch_filter initiated_at maybe_eb_worker_event_id = do
  conn <- ask
  [Only new_id] <- liftIO $ query conn sql (provider_id, branch_filter, initiated_at, maybe_eb_worker_event_id)
  return new_id
  where
    sql = Q.qjoin [
        "INSERT INTO ci_provider_build_scans"
      , Q.insertionValues [
          "provider"
        , "branch_filter"
        , "initiated_at"
        , "eb_worker_event"
        ]
      , "RETURNING id;"
      ]


storeMatches ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> [ScanPatterns.ScanMatch]
  -> IO Int64
storeMatches scan_resources (Builds.NewBuildStepId build_step_id) scoped_matches = do

  D.debugList [
      "Now storing"
    , show $ length scoped_matches
    , show build_step_id ++ "..."
    ]

  count <- executeMany conn insertion_sql $ map to_tuple scoped_matches

  D.debugList [
      "Finished storing"
    , show $ length scoped_matches
    , "matches."
    ]

  return count

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
    scan_id = ScanRecords.scan_id scan_resources

    to_tuple match = (
        scan_id
      , build_step_id
      , DbHelpers.db_id $ ScanPatterns.scanned_pattern match
      , ScanPatterns.line_number match_deets
      , ScanPatterns.line_text match_deets
      , DbHelpers._start $ ScanPatterns.span match_deets
      , DbHelpers._end $ ScanPatterns.span match_deets
      )
      where
        match_deets = ScanPatterns.match_details match

    insertion_sql = Q.qjoin [
        "INSERT INTO matches"
      , Q.insertionValues [
            "scan_id"
          , "build_step"
          , "pattern"
          , "line_number"
          , "line_text"
          , "span_start"
          , "span_end"
          ]
      ]


insertSingleCIProvider :: Connection -> String -> IO (DbHelpers.WithId String)
insertSingleCIProvider conn hostname = do
  rows <- query conn sql_query $ Only hostname
  row_id <- case rows of
    Only old_id:_ -> return old_id
    []              -> do
      [Only new_id] <- query conn sql_insert $ Only hostname
      return new_id
  return $ DbHelpers.WithId row_id hostname
  where
    sql_query = "SELECT id FROM ci_providers WHERE hostname = ? LIMIT 1;"
    sql_insert = Q.qjoin [
        "INSERT INTO ci_providers"
      , Q.insertionValues ["hostname"]
      , "RETURNING id;"
      ]


getAndStoreCIProviders ::
     Connection
  -> [(String, a)]
  -> IO [(a, DbHelpers.WithId String)]
getAndStoreCIProviders conn =
  mapM $ traverse (insertSingleCIProvider conn) . swap


data BeanstalkCronHeaders = BeanstalkCronHeaders {
    task_name    :: TL.Text
  , scheduled_at :: UTCTime
  , sender_id    :: TL.Text
  } deriving Show


insertEbWorkerStart ::
     Connection
  -> Text -- ^ web request path
  -> Text -- ^ label
  -> Maybe BeanstalkCronHeaders
  -> IO Int64
insertEbWorkerStart conn web_request_path label maybe_beanstalk_headers = do

  [Only record_id] <- query conn sql (
      web_request_path
    , label
    , task_name <$> maybe_beanstalk_headers
    , scheduled_at <$> maybe_beanstalk_headers
    , sender_id <$> maybe_beanstalk_headers
    )

  return record_id
  where
    sql = Q.qjoin [
        "INSERT INTO lambda_logging.eb_worker_event_start"
      , Q.insertionValues [
          "path"
        , "label"
        , "task_name"
        , "scheduled_at"
        , "sender_id"
        ]
      , "RETURNING id;"
      ]


insertEbWorkerFinish ::
     Connection
  -> Int64 -- ^ label
  -> IO ()
insertEbWorkerFinish conn start_id = do
  execute conn sql $ Only start_id
  return ()
  where
    sql = Q.qjoin [
        "INSERT INTO lambda_logging.eb_worker_event_finish"
      , Q.insertionValues ["start_id"]
      ]


updateUserOptOutSettings ::
     Bool
  -> SqlRead.AuthDbIO (Either Text SqlRead.OptOutResponse)
updateUserOptOutSettings posting_is_enabled = do
  SqlRead.AuthConnection conn (AuthStages.Username author) <- ask
  [x] <- liftIO $ query conn sql (author, not posting_is_enabled)
  return $ Right x
  where
    sql = Q.qjoin [
        "INSERT INTO pr_comment_posting_opt_outs"
      , Q.insertionValues [
          "username"
        , "disabled"
        ]
      , "ON CONFLICT (username) DO UPDATE"
      , "SET"
      , Q.list [
          "disabled = EXCLUDED.disabled"
        , "modification_count = pr_comment_posting_opt_outs.modification_count + 1"
        , "modified_at = now()"
        ]
      , "RETURNING"
      , Q.list [
          "username"
        , "disabled"
        , "modification_count"
        , "modified_at"
        ]
      ]


insertReceivedGithubStatus ::
     Connection
  -> Webhooks.GitHubStatusEvent
  -> IO Int64
insertReceivedGithubStatus conn (Webhooks.GitHubStatusEvent sha name description state target_url context created_at _) = do

  [Only record_id] <- query conn sql (sha, name, description, state, target_url, context, created_at)
  return record_id
  where
    sql = Q.qjoin [
        "INSERT INTO github_incoming_status_events"
      , Q.insertionValues [
          "sha1"
        , "name"
        , "description"
        , "state"
        , "target_url"
        , "context"
        , "created_at"
        ]
      , "RETURNING id;"
      ]


insertPostedGithubStatus ::
     Connection
  -> Builds.RawCommit
  -> DbHelpers.OwnerAndRepo
  -> ApiPost.StatusPostResult
  -> IO Int64
insertPostedGithubStatus conn (Builds.RawCommit git_sha1) (DbHelpers.OwnerAndRepo owner repo) (ApiPost.StatusPostResult id url state desc target_url context created_at updated_at) = do

  [Only record_id] <- query conn sql (id, git_sha1, owner, repo, url, state, desc, target_url, context, created_at, updated_at)
  return record_id
  where
    sql = Q.qjoin [
        "INSERT INTO created_github_statuses"
      , Q.insertionValues [
          "id"
        , "sha1"
        , "project"
        , "repo"
        , "url"
        , "state"
        , "description"
        , "target_url"
        , "context"
        , "created_at"
        , "updated_at"
        ]
      , "RETURNING id;"
      ]


commentBodyInsertionSql = Q.qjoin [
    "INSERT INTO created_pull_request_comment_revisions"
  , Q.insertionValues [
      "comment_id"
    , "body"
    , "updated_at"
    , "sha1"
    ]
  , "RETURNING id;"
  ]


insertPostedGithubComment ::
     Connection
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit -- ^ only used for logging
  -> Builds.PullRequestNumber
  -> ApiPost.CommentPostResult
  -> IO Int64
insertPostedGithubComment
    conn
    (DbHelpers.OwnerAndRepo owner repo)
    (Builds.RawCommit sha1)
    (Builds.PullRequestNumber pull_request_number)
    (ApiPost.CommentPostResult comment_id body created_at updated_at) = do

  execute conn comment_insertion_sql (comment_id, owner, repo, pull_request_number, created_at)

  [Only comment_revision_id] <- query conn commentBodyInsertionSql (comment_id, body, updated_at, sha1)
  return comment_revision_id

  where
    comment_insertion_sql = Q.qjoin [
        "INSERT INTO created_pull_request_comments"
      , Q.insertionValues [
          "comment_id"
        , "project"
        , "repo"
        , "pr_number"
        , "created_at"
        ]
      ]


markPostedGithubCommentAsDeleted ::
     Connection
  -> ApiPost.CommentId
  -> IO Int64
markPostedGithubCommentAsDeleted conn (ApiPost.CommentId comment_id) =
  execute conn sql (Only comment_id)
  where
    sql = "UPDATE created_pull_request_comments SET deleted = TRUE WHERE comment_id = ?;"


modifyPostedGithubComment ::
     Connection
  -> Builds.RawCommit -- ^ only used for logging
  -> ApiPost.CommentPostResult
  -> IO Int64
modifyPostedGithubComment
    conn
    (Builds.RawCommit sha1)
    (ApiPost.CommentPostResult comment_id body _created_at updated_at) = do

  [Only comment_revision_id] <- query conn commentBodyInsertionSql (comment_id, body, updated_at, sha1)
  return comment_revision_id


addPatternTag ::
     ScanPatterns.PatternId
  -> Text
  -> SqlRead.DbIO (Either Text Int64)
addPatternTag (ScanPatterns.PatternId pattern_id) tag = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (pattern_id, tag)
  where
    sql = Q.qjoin [
        "INSERT INTO pattern_tags"
      , Q.insertionValues [
          "pattern"
        , "tag"
        ]
      ]


removePatternTag ::
     ScanPatterns.PatternId
  -> Text
  -> SqlRead.DbIO (Either Text Int64)
removePatternTag (ScanPatterns.PatternId pattern_id) tag = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (pattern_id, tag)
  where
    sql = "DELETE FROM pattern_tags WHERE pattern = ? AND tag = ?;"


deleteCodeBreakage ::
     Int64
  -> SqlRead.DbIO (Either Text Int64)
deleteCodeBreakage cause_id = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (Only cause_id)
  where
    sql = "DELETE FROM code_breakage_cause WHERE id = ?;"


deleteSha1QueuePlaceholder ::
     Builds.RawCommit
  -> SqlRead.DbIO (Either Text Int64)
deleteSha1QueuePlaceholder (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (Only sha1)
  where
    sql = "DELETE FROM work_queues.queued_sha1_scans WHERE sha1 = ?;"


deleteCodeBreakageJob ::
     Int64
  -> Text
  -> SqlRead.DbIO (Either Text Int64)
deleteCodeBreakageJob cause_id job = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (cause_id, job)
  where
    sql = "DELETE FROM code_breakage_affected_jobs WHERE cause = ? AND job = ?;"


updateCodeBreakageDescription ::
     Int64
  -> Text
  -> SqlRead.DbIO (Either Text Int64)
updateCodeBreakageDescription cause_id description = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (description, cause_id)
  where
    sql = "UPDATE code_breakage_cause SET description = ? WHERE id = ?;"


updateCodeBreakageResolutionSha1 ::
     Int64
  -> Builds.RawCommit
  -> SqlRead.DbIO (Either Text Int64)
updateCodeBreakageResolutionSha1 cause_id (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (sha1, cause_id)
  where
    sql = "UPDATE code_breakage_resolution SET sha1 = ? WHERE cause = ?;"


updateCodeBreakageCauseSha1 ::
     Int64
  -> Builds.RawCommit
  -> SqlRead.DbIO (Either Text Int64)
updateCodeBreakageCauseSha1 cause_id (Builds.RawCommit sha1) = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (sha1, cause_id)
  where
    sql = "UPDATE code_breakage_cause SET sha1 = ? WHERE id = ?;"


deleteCodeBreakageResolution ::
     Int64
  -> SqlRead.DbIO (Either Text Int64)
deleteCodeBreakageResolution cause_id = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (Only cause_id)
  where
    sql = "DELETE FROM code_breakage_resolution WHERE cause = ?;"


addCodeBreakageJobName ::
     Int64 -- ^ cause
  -> Text -- ^ job name
  -> SqlRead.AuthDbIO (Either Text Int64)
addCodeBreakageJobName cause_id job_name = do
  SqlRead.AuthConnection conn (AuthStages.Username author) <- ask
  liftIO $ Right <$> execute conn job_insertion_sql (job_name, cause_id, author)
  where
    job_insertion_sql = Q.qjoin [
        "INSERT INTO code_breakage_affected_jobs"
      , Q.insertionValues [
          "job"
        , "cause"
        , "reporter"
        ]
      ]


updateCodeBreakageMode ::
     Int64 -- ^ cause
  -> Int64 -- ^ mode
  -> SqlRead.AuthDbIO (Either Text Int64)
updateCodeBreakageMode cause_id mode = do
  SqlRead.AuthConnection conn (AuthStages.Username author) <- ask
  liftIO $ Right <$> execute conn insertion_sql (cause_id, author, mode)
  where
    insertion_sql = Q.qjoin [
        "INSERT INTO master_failure_mode_attributions"
      , Q.insertionValues [
          "cause_id"
        , "reporter"
        , "mode_id"
        ]
      ]


storePullRequestStaticMetadata ::
     Builds.PullRequestNumber
  -> GithubApiFetch.PullRequestResponseSubset
  -> SqlRead.DbIO (Either Text Int64)
storePullRequestStaticMetadata pr_number pr_metadata = do
  conn <- ask
  liftIO $ Right <$> execute conn insertion_sql (GithubApiFetch.toDbFields pr_number pr_metadata)
  where
    insertion_sql = Q.qjoin [
        "INSERT INTO pull_request_static_metadata"
      , Q.insertionValues [
          "pr_number"
        , "github_user_login"
        , "base_repo_owner"
        , "base_repo_name"
        , "base_ref"
        , "head_repo_owner"
        , "head_repo_name"
        , "head_ref"
        ]
      ]


recordBlockedPRCommentPosting ::
     Builds.PullRequestNumber
  -> SqlRead.AuthDbIO (Either TL.Text Int64)
recordBlockedPRCommentPosting (Builds.PullRequestNumber pr_number) = do
  SqlRead.AuthConnection conn (AuthStages.Username author) <- ask
  liftIO $ Right <$> execute conn insertion_sql (author, pr_number)
  where
    insertion_sql = Q.qjoin [
        "INSERT INTO blocked_pr_comment_postings"
      , Q.insertionValues [
          "username"
        , "pr_number"
        ]
      ]


updatePatternDescription ::
     ScanPatterns.PatternId
  -> Text
  -> SqlRead.DbIO (Either Text Int64)
updatePatternDescription (ScanPatterns.PatternId pattern_id) description = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (description, pattern_id)
  where
    sql = "UPDATE patterns SET description = ? WHERE id = ?;"


updatePatternSpecificity ::
     ScanPatterns.PatternId
  -> Int
  -> SqlRead.DbIO (Either Text Int64)
updatePatternSpecificity (ScanPatterns.PatternId pattern_id) specificity = do
  conn <- ask
  liftIO $ Right <$> execute conn sql (specificity, pattern_id)
  where
    sql = "UPDATE patterns SET specificity = ? WHERE id = ?;"


insertSinglePattern ::
     Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> SqlRead.DbIO Int64
insertSinglePattern either_pattern = do
  conn <- ask
  liftIO $ do
    [Only pattern_id] <- case maybe_id of
      Nothing -> query conn pattern_insertion_sql (is_regex, pattern_text, description, is_retired, has_nondeterminisic_values, specificity, lines_from_end)
      Just record_id -> query conn pattern_insertion_with_id_sql (record_id :: Int64, is_regex, pattern_text, description, is_retired, has_nondeterminisic_values, specificity, lines_from_end)

    case maybe_timestamp of
      Just timestamp -> execute conn authorship_insertion_with_timestamp_sql (pattern_id, author, timestamp)
      Nothing -> execute conn authorship_insertion_sql (pattern_id, author)

    for_ tags $ \tag ->
      execute conn tag_insertion_sql (tag, pattern_id)

    for_ applicable_steps $ \applicable_step ->
      execute conn applicable_step_insertion_sql (applicable_step, pattern_id)

    return pattern_id

  where
    pattern_text = ScanPatterns.patternText expression_obj
    is_regex = ScanPatterns.isRegex expression_obj

    (ScanPatterns.NewPattern expression_obj description tags applicable_steps specificity is_retired lines_from_end) = pattern_obj

    (pattern_obj, AuthStages.Username author, maybe_timestamp, maybe_id) = case either_pattern of
      Left (patt_obj, username) -> (patt_obj, username, Nothing, Nothing)
      Right (DbHelpers.WithAuthorship auth created_time (DbHelpers.WithId _record_id patt_obj)) -> (patt_obj, AuthStages.Username auth, Just created_time, Nothing)

    has_nondeterminisic_values = case expression_obj of
      ScanPatterns.RegularExpression _ has_nondeterministic -> has_nondeterministic
      ScanPatterns.LiteralExpression _                       -> False

    pattern_insertion_sql = Q.qjoin [
        "INSERT INTO patterns"
      , Q.insertionValues [
          "regex"
        , "expression"
        , "description"
        , "is_retired"
        , "has_nondeterministic_values"
        , "specificity"
        , "lines_from_end"
        ]
      , "RETURNING id;"
      ]

    pattern_insertion_with_id_sql = Q.qjoin [
        "INSERT INTO patterns"
      , Q.insertionValues [
          "id"
        , "regex"
        , "expression"
        , "description"
        , "is_retired"
        , "has_nondeterministic_values"
        , "specificity"
        , "lines_from_end"
        ]
      , "RETURNING id;"
      ]

    tag_insertion_sql = Q.qjoin [
        "INSERT INTO pattern_tags"
      , Q.insertionValues [
          "tag"
        , "pattern"
        ]
      ]

    authorship_insertion_with_timestamp_sql = Q.qjoin [
        "INSERT INTO pattern_authorship"
      , Q.insertionValues [
          "pattern"
        , "author"
        , "created"
        ]
      ]

    authorship_insertion_sql = Q.qjoin [
        "INSERT INTO pattern_authorship"
      , Q.insertionValues [
          "pattern"
        , "author"
        ]
      ]

    applicable_step_insertion_sql = Q.qjoin [
        "INSERT INTO pattern_step_applicability"
      , Q.insertionValues [
          "step_name"
        , "pattern"
        ]
      ]


restorePatterns ::
     [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
  -> SqlRead.DbIO (Either Text [Int64])
restorePatterns pattern_list = do
  eithers <- for pattern_list $ apiNewPattern . Right
  return $ sequenceA eithers


stepFailureToTuple ::
     (Builds.UniversalBuildId, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
  -> (Maybe Text, Bool, Int64, Int)
stepFailureToTuple (Builds.UniversalBuildId universal_buildnum, visitation_result) = case visitation_result of
  Right _ -> (Nothing, False, universal_buildnum, -1)
  Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure stepname step_index mode)) -> let
    is_timeout = case mode of
      Builds.BuildTimeoutFailure              -> True
      Builds.ScannableFailure _failure_output -> False
    in (Just stepname, is_timeout, universal_buildnum, step_index)


populatePresumedStableBranches :: Connection -> [Text] -> IO Int64
populatePresumedStableBranches conn =
  executeMany conn sql . map Only
  where
    sql = Q.qjoin [
        "INSERT INTO presumed_stable_branches"
      , Q.insertionValues ["branch"]
      ]


cacheAllMergeBases ::
     Connection
  -> Set Builds.RawCommit
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> [Builds.RawCommit]
  -> IO ()
cacheAllMergeBases conn all_master_commits access_token owned_repo commits =

  mapM_ f $ zip [1..] commits

  where
    f (i, x) = do
      findMasterAncestorWithPrecomputation
        (Just all_master_commits)
        conn
        access_token
        owned_repo
        StoreToCache
        x

      D.debugList [
        "Progress:"
        , show i
        , "/"
        , show $ length commits
        ]


storeLogInfo ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> ScanRecords.LogInfo
  -> IO Int64
storeLogInfo
    scan_resources
    (Builds.NewBuildStepId step_id)
    (ScanRecords.LogInfo byte_count line_count log_content modified_by_ansi_stripping was_truncated_for_size) =

  execute conn sql (
      step_id
    , line_count
    , byte_count
    , PGArray log_content
    , modified_by_ansi_stripping
    , was_truncated_for_size
    )

  where
    sql = Q.qjoin [
        "INSERT INTO log_metadata"
      , Q.insertionValues [
          "step"
        , "line_count"
        , "byte_count"
        , "content_lines"
        , "modified_by_ansi_stripping"
        , "was_truncated_for_size"
        ]
      , "ON CONFLICT (step)"
      , "DO UPDATE"
      , "SET"
      , Q.list [
          "line_count = EXCLUDED.line_count"
        , "byte_count = EXCLUDED.byte_count"
        , "content_lines = EXCLUDED.content_lines"
        , "modified_by_ansi_stripping = EXCLUDED.modified_by_ansi_stripping"
        , "was_truncated_for_size = EXCLUDED.was_truncated_for_size"
        ]
      ]

    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insertLatestPatternBuildScan ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> Int64
  -> IO ()
insertLatestPatternBuildScan
    scan_resources
    (Builds.NewBuildStepId step_id)
    maximum_pattern_id = do

  D.debugList [
      "Now storing largest scanned pattern ID:"
    , show maximum_pattern_id
    ]

  execute conn sql (ScanRecords.scan_id scan_resources, step_id, maximum_pattern_id)

  D.debugList [
      "Stored largest scanned pattern ID"
    , show maximum_pattern_id ++ "."
    ]

  where
    sql = Q.qjoin [
        "INSERT INTO scanned_patterns"
      , Q.insertionValues [
          "scan"
        , "step_id"
        , "newest_pattern"
        ]
      ]

    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insertBuildVisitation ::
     ScanRecords.ScanCatchupResources
  -> (DbHelpers.WithId Builds.UniversalBuild, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
  -> IO Builds.BuildStepId
insertBuildVisitation scan_resources (ubuild, visitation_result) = do

  [Only step_id] <- query conn insertion_sql $
    stepFailureToTuple (Builds.UniversalBuildId universal_build_id, visitation_result)

  return $ Builds.NewBuildStepId step_id

  where
    universal_build_id = DbHelpers.db_id ubuild

    insertion_sql = Q.qjoin [
        "INSERT INTO build_steps"
      , Q.insertionValues [
          "name"
        , "is_timeout"
        , "universal_build"
        , "step_index"
        ]
      , "ON CONFLICT"
      , "ON CONSTRAINT build_steps_universal_build_step_index_key"
      , "DO UPDATE SET"
      , Q.list [
          "name = excluded.name"
        , "is_timeout = excluded.is_timeout"
        ]
      , "RETURNING id;"
      ]

    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insertScanId ::
     Connection
  -> Maybe AuthStages.Username
  -> ScanPatterns.PatternId
  -> IO Int64
insertScanId conn maybe_initiator (ScanPatterns.PatternId pattern_id)  = do
  [Only pattern_id] <- query conn sql (pattern_id, inititator)
  return pattern_id
  where
    inititator = fmap (\(AuthStages.Username x) -> x) maybe_initiator
    sql = Q.qjoin [
        "INSERT INTO scans"
      , Q.insertionValues [
          "latest_pattern_id"
        , "initiator"
        ]
      , "RETURNING id;"
      ]


reportBreakage ::
     SqlRead.AuthConnection
  -> String
  -> Int64
  -> Bool
  -> Builds.RawCommit
  -> Builds.RawCommit
  -> Text
  -> ExceptT Text IO Int64
reportBreakage
    auth_conn@(SqlRead.AuthConnection conn user_alias)
    jobs_delimited
    failure_mode_id
    is_still_ongoing
    last_affected_sha1
    breakage_sha1
    notes = do

  cause_id <- ExceptT $ apiCodeBreakageCauseInsert
    conn
    breakage_report
    jobs_list

  ExceptT $ runReaderT
    (updateCodeBreakageMode cause_id failure_mode_id)
    auth_conn

  liftIO $ unless is_still_ongoing $ do
    runExceptT $ do

      (Builds.RawCommit resolution_sha1) <- ExceptT $
        SqlRead.getNextMasterCommit conn last_affected_sha1

      ExceptT $ apiCodeBreakageResolutionInsert conn $
        Breakages.NewResolutionReport
          resolution_sha1
          cause_id
          user_alias

    return ()

  return cause_id

  where
    breakage_report = Breakages.NewBreakageReport
      breakage_sha1
      notes
      user_alias

    jobs_list = DbHelpers.cleanSemicolonDelimitedList jobs_delimited


apiCodeBreakageCauseInsert ::
     Connection
  -> Breakages.BreakageReport
  -> [Text] -- ^ job names
  -> IO (Either Text Int64)
apiCodeBreakageCauseInsert
    conn
    (Breakages.NewBreakageReport (Builds.RawCommit sha1) description (AuthStages.Username author_username))
    job_names =

  catchViolation catcher $ do

    [Only report_id] <- query conn insertion_sql (sha1, description, author_username)

    for_ job_names $ \job ->
      execute conn job_insertion_sql (job, report_id, author_username)

    return $ Right report_id

  where
    insertion_sql = Q.qjoin [
        "INSERT INTO code_breakage_cause"
      , Q.insertionValues [
          "sha1"
        , "description"
        , "reporter"
        ]
      , "RETURNING id;"
      ]

    job_insertion_sql = Q.qjoin [
        "INSERT INTO code_breakage_affected_jobs"
      , Q.insertionValues [
          "job"
        , "cause"
        , "reporter"
        ]
      ]

    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


apiCodeBreakageResolutionInsertMultiple sha1 cause_ids_delimited = do

  SqlRead.AuthConnection conn user_alias <- ask
  let gen_resolution_report cause_id = Breakages.NewResolutionReport
        sha1
        cause_id
        user_alias

  liftIO $ do
    insertion_eithers <- mapM (apiCodeBreakageResolutionInsert conn . gen_resolution_report) cause_id_list
    return $ sequenceA insertion_eithers

  where
    cause_id_list = map (read . T.unpack) $
      DbHelpers.cleanSemicolonDelimitedList cause_ids_delimited


apiCodeBreakageResolutionInsert ::
     Connection
  -> Breakages.ResolutionReport
  -> IO (Either Text Int64)
apiCodeBreakageResolutionInsert
    conn
    (Breakages.NewResolutionReport sha1 cause_id (AuthStages.Username author_username)) =

  -- TODO: Ensure that the commit index of the resolution is strictly higher
  -- than the commit index of the cause

  catchViolation catcher $ do

    [Only report_id] <- query conn insertion_sql (sha1, cause_id, author_username)
    return $ Right report_id

  where
    insertion_sql = Q.qjoin [
        "INSERT INTO code_breakage_resolution"
      , Q.insertionValues [
          "sha1"
        , "cause"
        , "reporter"
        ]
      , "RETURNING id;"
      ]

    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


retirePattern ::
     Connection
  -> ScanPatterns.PatternId
  -> IO ()
retirePattern conn (ScanPatterns.PatternId pattern_id) = do
  execute conn sql (True, pattern_id)
  return ()
  where
    sql = "UPDATE patterns SET is_retired = ? WHERE id = ?;"


data PatternFieldOverrides = PatternFieldOverrides {
    pat_expression       :: Maybe Text
  , pat_is_regex         :: Maybe Bool
  , pat_applicable_steps :: Maybe [Text]
  , pat_lines_from_end   :: Maybe Int
  }


copyPattern ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> AuthStages.Username
  -> PatternFieldOverrides
  -> IO (Either Text Int64)
copyPattern
    conn_data
    pattern_id@(ScanPatterns.PatternId pat_id)
    username
    field_overrides = do

  conn <- DbHelpers.get_connection conn_data
  pattern_rows <- query conn sql $ Only pat_id

  runExceptT $ do

    p <- except $ maybeToEither
      (T.pack $ unwords ["Pattern with ID", show pat_id, "not found."])
      (Safe.headMay pattern_rows)

    let (   p_is_regex
          , p_has_nondeterministic_values
          , p_expression_text
          , p_description
          , p_tags_array
          , p_steps_array
          , p_specificity
          , p_maybe_lines_from_end) = p

        expression_text = Maybe.fromMaybe p_expression_text $ pat_expression field_overrides
        is_regex = Maybe.fromMaybe p_is_regex $ pat_is_regex field_overrides

        new_pattern = ScanPatterns.NewPattern
          (ScanPatterns.toMatchExpression is_regex expression_text p_has_nondeterministic_values)
          p_description
          (fromPGArray p_tags_array)
          (Maybe.fromMaybe (fromPGArray p_steps_array) $ pat_applicable_steps field_overrides)
          p_specificity
          False
          (pat_lines_from_end field_overrides <|> p_maybe_lines_from_end)

    ExceptT $ do
      new_id <- runReaderT (apiNewPattern $ Left (new_pattern, username)) conn
      retirePattern conn pattern_id
      return new_id

  where
    sql = Q.qjoin [
        "SELECT"
      , Q.list [
          "regex"
        , "has_nondeterministic_values"
        , "expression"
        , "description"
        , "tags_array"
        , "steps_array"
        , "specificity"
        , "lines_from_end"
        ]
      , "FROM patterns_augmented"
      , "WHERE id = ?;"
      ]


promoteMatch ::
     MatchOccurrences.MatchId
  -> SqlRead.AuthDbIO (Either Text Int64)
promoteMatch (MatchOccurrences.MatchId match_id) = do
  SqlRead.AuthConnection conn (AuthStages.Username username) <- ask
  liftIO $ catchViolation catcher $ do
    [Only match_id] <- query conn sql (match_id, username)
    return match_id
  where
    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e

    sql = Q.qjoin [
        "INSERT INTO build_failure_elaborations"
      , Q.insertionValues [
          "match"
        , "author"
        ]
      , "RETURNING match;"
      ]


apiNewPatternWrapped ::
     ScanPatterns.Pattern
  -> SqlRead.AuthDbIO (Either Text Int64)
apiNewPatternWrapped new_pattern = do
  SqlRead.AuthConnection conn user <- ask
  liftIO $ runReaderT (apiNewPattern $ Left (new_pattern, user)) conn


-- | TODO Is there a nicer way to propagate
-- the ReaderT inside the catchViolation function
-- without deconstructing/reconstructing it?
apiNewPattern ::
     Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> SqlRead.DbIO (Either Text Int64)
apiNewPattern new_pattern = do
  conn <- ask
  liftIO $ catchViolation catcher $ do
    record_id <- runReaderT (insertSinglePattern new_pattern) conn
    return $ Right record_id

  where
    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e
