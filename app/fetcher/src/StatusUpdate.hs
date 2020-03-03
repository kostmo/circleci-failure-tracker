{-# LANGUAGE OverloadedStrings #-}

module StatusUpdate (
    githubEventEndpoint
  , readGitHubStatusesAndScanAndPostSummaryForCommit
  , postCommitSummaryStatus
  , fetchCommitPageInfo
  , wrappedScanAndPostCommit
  , SuccessRecordStorageMode (..)
  , ScanLogsMode (..)
  , viableBranchName
  , statementTimeoutSeconds
  ) where

import           Control.Monad                 (guard, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except    (ExceptT (ExceptT), except,
                                                runExceptT)
import           Control.Monad.Trans.Reader    (runReaderT)
import           Data.Bifunctor                (first)
import qualified Data.ByteString.Lazy          as LBS
import           Data.Foldable                 (for_)
import qualified Data.HashMap.Strict           as HashMap
import           Data.List                     (filter, partition)
import           Data.List.Split               (splitOn)
import qualified Data.Maybe                    as Maybe
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Time.Clock               as Clock
import           Data.Traversable              (for)
import           Data.Tuple                    (swap)
import           Database.PostgreSQL.Simple    (Connection)
import           GHC.Int                       (Int64)
import qualified GitHub.Data.Webhooks.Validate as GHValidate
import qualified Network.OAuth.OAuth2          as OAuth2
import qualified Network.URI                   as URI
import qualified Safe
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S
import           Web.Scotty.Internal.Types     (ActionT)

import qualified ApiPost
import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified CircleApi
import qualified CircleAuth
import qualified CommentRender
import qualified CommentRenderCommon
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified DebugUtils                    as D
import qualified GadgitFetch
import qualified GithubApiFetch
import qualified GitRev
import qualified Markdown                      as M
import qualified MatchOccurrences
import qualified MyUtils
import qualified PullRequestWebhooks
import qualified PushWebhooks
import qualified Scanning
import qualified ScanRecords
import qualified Sql.Read                      as SqlRead
import qualified Sql.Update                    as SqlUpdate
import qualified Sql.Write                     as SqlWrite
import qualified StatusEventQuery
import qualified StatusUpdateTypes
import qualified UnmatchedBuilds
import qualified Webhooks


-- | 2 minutes
statementTimeoutSeconds :: Integer
statementTimeoutSeconds = 120


viableBranchName :: Text
viableBranchName = "viable/strict"


pullRequestEventActionSynchronize :: LT.Text
pullRequestEventActionSynchronize = "synchronize"


fullMasterRefName :: Text
fullMasterRefName = "refs/heads/" <> Constants.masterName


circleciDomain :: String
circleciDomain = "circleci.com"


circleCIContextPrefix :: Text
circleCIContextPrefix = "ci/circleci: "


gitHubStatusFailureString = "failure"


gitHubStatusSuccessString = "success"


conclusiveStatuses = [
    gitHubStatusFailureString
  , gitHubStatusSuccessString
  ]


groupStatusesByHostname ::
     [StatusEventQuery.GitHubStatusEventGetter]
  -> [(String, [StatusEventQuery.GitHubStatusEventGetter])]
groupStatusesByHostname =
  MyUtils.binTuplesByFirst . map swap . Maybe.mapMaybe (sequenceA . MyUtils.derivePair get_hostname)
  where
    get_hostname s = do
      parsed_uri <- URI.parseURI $ LT.unpack $ StatusEventQuery._target_url s
      authority <- URI.uriAuthority parsed_uri
      return $ URI.uriRegName authority


-- | XXX Note that we are obtaining the build metadata from an "unofficial"
-- source; the queued_at time is inaccurate and the branch name is empty.
getCircleciFailure ::
     Builds.RawCommit
  -> StatusEventQuery.GitHubStatusEventGetter
  -> Maybe Builds.Build
getCircleciFailure sha1 event_setter = do

  guard $ circleCIContextPrefix `T.isPrefixOf` context_text
  parsed_uri <- URI.parseURI $ LT.unpack url_text

  last_segment <- Safe.lastMay $ splitOn "/" $ URI.uriPath parsed_uri
  build_number <- readMaybe last_segment
  return $ Builds.NewBuild
    (Builds.NewBuildNumber build_number)
    sha1
    current_time
    build_name
    Nothing
    Nothing
    Nothing

  where
    context_text = LT.toStrict context
    build_name = T.drop (T.length circleCIContextPrefix) context_text

    current_time = StatusEventQuery._created_at event_setter
    context = StatusEventQuery._context event_setter
    url_text = StatusEventQuery._target_url event_setter


-- | TODO return Left for each universal build that
-- violated its uniqueness constraint
--
-- TODO Replace the inside logic with SqlWite.storeHelper
storeUniversalBuilds ::
     Connection
  -> Builds.RawCommit
  -> [([StatusEventQuery.GitHubStatusEventGetter], DbHelpers.WithId String)]
  -> IO [(Builds.StorableBuild, (StatusEventQuery.GitHubStatusEventGetter, String))]
storeUniversalBuilds conn commit statuses_by_ci_providers = do

  result_lists <- for statuses_by_ci_providers $ \(statuses, provider_with_id) -> do

    {-
    -- XXX This does not work, because the return value of this function
    -- requires *all* the builds for this commit, not just the ones
    -- that were stored to the DB in this invoation.

    let f status_event = sequenceA (status_event, extractUniversalBuild
            commit
            provider_with_id
            status_event)

    let build_pairs = Maybe.mapMaybe f statuses
        prepped_input = map (\(status_event, (sub_build, uni_build)) -> (status_event, (sub_build, Builds.succeeded uni_build))) build_pairs


    output <- SqlWrite.storeHelper conn prepped_input

    let prepped_output = map (\(status_event, storable_build) -> (storable_build, (status_event, DbHelpers.record provider_with_id))) output

    return prepped_output

    -}
    result_maybe_list <- for statuses $ \status_event -> do

      let maybe_universal_build = extractUniversalBuild
            commit
            provider_with_id
            status_event

      case maybe_universal_build of
        Nothing -> return Nothing
        Just (sub_build, uni_build) -> do

          stored_uni_build <- SqlWrite.insertSingleUniversalBuild conn $
            Builds.UniBuildWithJob uni_build $ Builds.job_name sub_build

          return $ Just (
              Builds.StorableBuild stored_uni_build sub_build
            , (status_event, DbHelpers.record provider_with_id)
            )

    return $ Maybe.catMaybes result_maybe_list

  return $ concat result_lists


extractUniversalBuild ::
     Builds.RawCommit
  -> DbHelpers.WithId String -- ^ CI provider domain and ID
  -> StatusEventQuery.GitHubStatusEventGetter
  -> Maybe (Builds.Build, Builds.UniversalBuild)
extractUniversalBuild commit provider_with_id status_object = case DbHelpers.record provider_with_id of
  "circleci.com"   -> do
    circle_build <- getCircleciFailure commit status_object
    let uni_build = Builds.UniversalBuild
          (Builds.build_id circle_build)
          (DbHelpers.db_id provider_with_id)
          ""
          did_succeed
          commit
    return (circle_build, uni_build)

  "ci.pytorch.org" -> Nothing
  "travis-ci.org"  -> Nothing
  _                -> Nothing

  where
    did_succeed = StatusEventQuery._state status_object == gitHubStatusSuccessString


getBuildsFromGithub ::
     ScanRecords.FetchingResources
  -> DbHelpers.OwnerAndRepo
  -> SuccessRecordStorageMode
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ([Builds.UniversalBuildId], Int)
getBuildsFromGithub
    fetch_resources
    owned_repo
    store_provider_specific_success_records
    sha1 = do

  build_statuses_list_any_source <- ExceptT $ GithubApiFetch.getBuildStatuses
    (CircleAuth.token $ ScanRecords.github_auth_token fetch_resources)
    owned_repo
    sha1

  liftIO $ D.debugList [
      "Build statuses count:"
    , show $ length build_statuses_list_any_source
    ]

  let succeeded_or_failed_statuses = filter ((`elem` conclusiveStatuses) . StatusEventQuery._state) build_statuses_list_any_source

      statuses_by_hostname = groupStatusesByHostname succeeded_or_failed_statuses

  statuses_by_ci_providers <- liftIO $ SqlWrite.getAndStoreCIProviders conn statuses_by_hostname

  liftIO $ D.debugList ["HERE 1A"]

  -- Only store succeeded or failed builds; ignore pending or aborted
  stored_build_tuples <- liftIO $ storeUniversalBuilds
    conn
    sha1
    statuses_by_ci_providers

  liftIO $ D.debugList ["HERE 1B"]

  let circleci_builds_and_statuses = filter ((== circleciDomain) . snd . snd) stored_build_tuples

      filter_by_status stat = filter $ (== stat) . StatusEventQuery._state . fst . snd

      circleci_failed_builds = map fst $ filter_by_status gitHubStatusFailureString circleci_builds_and_statuses

      -- TODO: May want to use the CircleCI API to retrieve more info
      -- on these builds.
      -- Currently, the extra fields available from CircleCI
      -- are only populated through the "BuildRetrieval.updateCircleCIBuildsList"
      -- function, which is manually invoked.
      circleci_successful_builds = map fst $ filter_by_status gitHubStatusSuccessString circleci_builds_and_statuses

      second_level_storable_builds = case store_provider_specific_success_records of
        StatusUpdate.ShouldStoreDetailedSuccessRecords -> circleci_failed_builds ++ circleci_successful_builds
        StatusUpdate.NoStoreDetailedSuccessRecords -> circleci_failed_builds

      scannable_build_numbers = map (Builds.UniversalBuildId . DbHelpers.db_id . Builds.universal_build) circleci_failed_builds

      circleci_failcount = length circleci_failed_builds

  liftIO $ D.debugList [
      "Failed CircleCI build count:"
    , show circleci_failcount
    ]


  liftIO $ flip runReaderT conn $ SqlWrite.storeBuildsList Nothing $
    map storable_build_to_universal second_level_storable_builds

  return (scannable_build_numbers, circleci_failcount)

  where
    conn = ScanRecords.db_conn fetch_resources
    storable_build_to_universal (Builds.StorableBuild (DbHelpers.WithId ubuild_id _ubuild) rbuild) =
      DbHelpers.WithTypedId (Builds.UniversalBuildId ubuild_id) rbuild


scanAndPost ::
     ScanRecords.ScanCatchupResources
  -> Scanning.RevisitationMode
  -> [Builds.UniversalBuildId]
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ()
scanAndPost
    scan_resources
    scan_revisitation_mode
    scannable_build_numbers
    owned_repo
    sha1 = do

  liftIO $ do

    Scanning.scanBuilds
      scan_resources
      Scanning.OnlyUnscannedPatterns
      scan_revisitation_mode
      Scanning.NoRefetchLog
      (Left $ Set.fromList scannable_build_numbers)

    D.debugList ["About to enter postCommitSummaryStatus"]

  postCommitSummaryStatus
    (ScanRecords.fetching scan_resources)
    owned_repo
    sha1


fetchCommitPageInfo ::
     SqlUpdate.UpstreamBreakagesInfo
  -> Builds.RawCommit
  -> GitRev.GitSha1
  -> SqlRead.DbIO (Either Text StatusUpdateTypes.CommitPageInfo)
fetchCommitPageInfo pre_broken_info sha1 validated_sha1 = runExceptT $ do

  liftIO $ D.debugStr "Fetching revision builds"
  DbHelpers.BenchmarkedResponse _ revision_builds <- ExceptT $
    SqlRead.getRevisionBuilds validated_sha1

  -- Partition builds between upstream and non-upstream breakages
  let f = MyUtils.derivePair $ (`HashMap.lookup` pre_broken_jobs_map) . Builds.job_name . Builds.build_record . CommitBuilds._build . CommitBuilds._commit_build

      (upstream_breakages, non_upstream_breakages_raw) = partition (not . null . snd) $ map f revision_builds
      paired_upstream_breakages = Maybe.mapMaybe sequenceA upstream_breakages

      non_upstream_breakages = map fst non_upstream_breakages_raw

  matched_builds_with_log_context <- for non_upstream_breakages $ \x ->
    ExceptT $ (fmap . fmap) (\y -> (x, CommitBuilds.BuildWithLogContext (CommitBuilds._commit_build x) y)) $
      SqlRead.logContextFunc 0
        (MatchOccurrences._match_id $ CommitBuilds._match $ CommitBuilds._commit_build x)
        CommentRender.pullRequestCommentsLogContextLineCount

  liftIO $ D.debugStr "Fetching unmatched commit builds..."


  -- Note: this function does not distinguish between upstream/non-upstream
  -- builds, so we post-process to exclude upstream builds
  unmatched_builds <- ExceptT $ SqlRead.apiUnmatchedCommitBuilds sha1
  let g = not . (`HashMap.member` pre_broken_jobs_map) . UnmatchedBuilds._job_name
      nonupstream_unmatched_builds = filter g unmatched_builds


  liftIO $ D.debugStr "Finishing fetchCommitPageInfo."

  let pattern_matched_builds_partition = StatusUpdateTypes.partitionMatchedBuilds matched_builds_with_log_context

  return $ StatusUpdateTypes.NewCommitPageInfo
    paired_upstream_breakages
    (StatusUpdateTypes.NewNonUpstreamBuildPartition pattern_matched_builds_partition nonupstream_unmatched_builds)

  where
    pre_broken_jobs_map = SqlUpdate.inferred_upstream_breakages_by_job pre_broken_info


postCommitSummaryStatus ::
     ScanRecords.FetchingResources
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ()
postCommitSummaryStatus
    fetching_resources
    owned_repo
    sha1@(Builds.RawCommit commit_sha1_text) = do

  liftIO $ D.debugStr "Checkpoint A"

  circleci_failed_job_names <- ExceptT $ flip runReaderT conn $
    SqlRead.getFailedCircleCIJobNames sha1

  liftIO $ D.debugList [
      "CircleCI failed job names:"
    , show circleci_failed_job_names
    ]

  liftIO $ D.debugStr "Checkpoint B"

  upstream_breakages_info <- ExceptT $
    first LT.fromStrict <$> runReaderT (SqlUpdate.findKnownBuildBreakages
      access_token
      owned_repo
      sha1) conn

  liftIO $ D.debugStr "Checkpoint C"

  validated_sha1 <- except $ first LT.fromStrict $ GitRev.validateSha1 commit_sha1_text

  liftIO $ D.debugStr "Checkpoint D"

  commit_page_info <- ExceptT $ first LT.fromStrict <$>
    runReaderT (fetchCommitPageInfo upstream_breakages_info sha1 validated_sha1) conn

  liftIO $ D.debugStr "Checkpoint E"

  x <- postCommitSummaryStatusInner
    circleci_failed_job_names
    upstream_breakages_info
    commit_page_info
    conn
    access_token
    owned_repo
    sha1

  liftIO $ D.debugStr "Checkpoint Z"
  return x

  where
    conn = ScanRecords.db_conn fetching_resources
    access_token = CircleAuth.token $ ScanRecords.github_auth_token fetching_resources


fetchAndCachePrAuthor ::
     Connection
  -> OAuth2.AccessToken
  -> Builds.PullRequestNumber
  -> ExceptT LT.Text IO AuthStages.Username
fetchAndCachePrAuthor conn access_token pr_number = do

  maybe_pr_author <- liftIO $ flip runReaderT conn $ SqlRead.getCachedPullRequestAuthor pr_number

  case maybe_pr_author of
    Just author -> do
      liftIO $ D.debugStr "Fetched PR author from database cache"
      return author
    Nothing -> do
      (pr_author, pr_metadata_obj) <- ExceptT $ first snd <$>
        GithubApiFetch.getPullRequestAuthor access_token pr_number

      liftIO $ do
        D.debugStr "Fetched PR author from GitHub API"
        runReaderT (SqlWrite.storePullRequestStaticMetadata pr_number pr_metadata_obj) conn
        D.debugStr "Stored PR author and other metadata in database"
      return pr_author


postCommitSummaryStatusInner ::
     [Text] -- ^ circleci failed job list
  -> SqlUpdate.UpstreamBreakagesInfo
  -> StatusUpdateTypes.CommitPageInfo
  -> Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ()
postCommitSummaryStatusInner
    circleci_fail_joblist
    upstream_breakages_info
    commit_page_info
    conn
    access_token
    owned_repo
    sha1 = do

  liftIO $ D.debugStr "Checkpoint F"

  -- XXX Is the following advice still applicable??
  --
  --   We're examining statuses on both failed and successful
  --   build notifications, which can add up to a lot of activity.
  --   We only should re-post our summary status if it will change what
  --   was already posted, since we don't want GitHub to throttle our requests.

  containing_pr_list <- lookupPullRequestsByHeadCommit conn sha1

  when (null containing_pr_list) $
    liftIO $ D.debugList [
        "No Pull Requests have HEAD commit of"
      , show sha1
      ]

  ancestry_result <- ExceptT $ fmap (first LT.pack) $ GadgitFetch.getIsAncestor $
    GadgitFetch.RefAncestryProposition merge_base_commit_text viableBranchName

  let middle_sections = CommentRender.generateMiddleSections
        ancestry_result
        build_summary_stats
        commit_page_info
        sha1

  for_ containing_pr_list $ \pr_number ->
    handleCommentPostingOptOut pr_number $ do
      maybe_previous_pr_comment <- liftIO $ flip runReaderT conn $
        SqlRead.getPostedCommentForPR pr_number

      case maybe_previous_pr_comment of
        Nothing -> postInitialComment
          access_token
          owned_repo
          conn
          sha1
          False
          middle_sections
          pr_number

        Just previous_pr_comment -> updateCommentOrFallback
          access_token
          owned_repo
          conn
          sha1
          False
          middle_sections
          pr_number
          previous_pr_comment

  where
  build_summary_stats = StatusUpdateTypes.NewBuildSummaryStats
    upstream_breakages_info
    circleci_fail_joblist

  Builds.RawCommit merge_base_commit_text = SqlUpdate.merge_base upstream_breakages_info


  handleCommentPostingOptOut pr_number f = do
    pr_author <- fetchAndCachePrAuthor conn access_token pr_number

    can_post_comments <- ExceptT $ SqlRead.canPostPullRequestComments conn pr_author
    if can_post_comments
      then f
      else ExceptT $ runReaderT (SqlWrite.recordBlockedPRCommentPosting pr_number) $
        SqlRead.AuthConnection conn pr_author


-- | Falls back to Gadgit webservice if database lookup did not find anything
lookupPullRequestsByHeadCommit ::
     Connection
  -> Builds.RawCommit
  -> ExceptT LT.Text IO [Builds.PullRequestNumber]
lookupPullRequestsByHeadCommit conn sha1 = do

  found_prs <- liftIO $ flip runReaderT conn $ SqlRead.getPullRequestsByCurrentHead sha1

  if null found_prs
    then ExceptT $ first LT.pack <$> GadgitFetch.getContainingPRs sha1
    else return found_prs


postInitialComment ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Connection
  -> Builds.RawCommit
  -> Bool -- ^ was new push
  -> CommentRenderCommon.PrCommentPayload
  -> Builds.PullRequestNumber
  -> ExceptT LT.Text IO Int64
postInitialComment
    access_token
    owned_repo
    conn
    sha1
    was_new_push
    pr_comment_payload
    pr_number = do

  comment_post_result <- ExceptT $ ApiPost.postPullRequestComment
    access_token
    owned_repo
    pr_number
    pr_comment_text

  liftIO $ SqlWrite.insertPostedGithubComment
    conn
    owned_repo
    sha1
    pr_number
    was_new_push
    pr_comment_payload
    comment_post_result

  where
    pr_comment_text = CommentRender.generateCommentMarkdown
      Nothing
      pr_comment_payload
      sha1


updateCommentOrFallback ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Connection
  -> Builds.RawCommit
  -> Bool -- ^ was new push
  -> CommentRenderCommon.PrCommentPayload
  -> Builds.PullRequestNumber
  -> SqlRead.PostedPRComment
  -> ExceptT LT.Text IO Int64
updateCommentOrFallback
    access_token
    owned_repo
    conn
    sha1
    was_new_push
    pr_comment_payload
    pr_number
    previous_pr_comment = do

  either_comment_update_result <- liftIO $ ApiPost.updatePullRequestComment
    access_token
    owned_repo
    comment_id
    pr_comment_text

  case either_comment_update_result of
    Right comment_update_result ->
      liftIO $ SqlWrite.modifyPostedGithubComment
        conn
        sha1
        was_new_push
        pr_comment_payload
        comment_update_result

    -- If the comment was deleted, we need to re-post one.
    Left "Not Found" -> do
      liftIO $ D.debugStr "Comment was deleted. Posting a new one..."

      -- Mark our database entry as stale
      liftIO $ SqlWrite.markPostedGithubCommentAsDeleted conn comment_id

      postInitialComment
        access_token
        owned_repo
        conn
        sha1
        was_new_push
        pr_comment_payload
        pr_number

    Left other_failure_message -> except $ Left other_failure_message

  where
    pr_comment_text = CommentRender.generateCommentMarkdown
      (Just previous_pr_comment)
      pr_comment_payload
      sha1

    comment_id = ApiPost.CommentId $ SqlRead._comment_id previous_pr_comment


wipeCommentForUpdatedPr ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Connection
  -> SqlRead.PostedPRComment
  -> Builds.PullRequestNumber
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ()
wipeCommentForUpdatedPr
    access_token
    owned_repo
    conn
    previous_pr_comment
    pr_number
    new_pr_head_commit@(Builds.RawCommit sha1_text) = do

  updateCommentOrFallback
    access_token
    owned_repo
    conn
    new_pr_head_commit
    True
    (CommentRenderCommon.NewPrCommentPayload [[middle_sections]] True True)
    pr_number
    previous_pr_comment

  return ()
  where
    middle_sections = M.italic $ T.unwords [
        "Commit"
      , T.take Constants.gitCommitPrefixLength sha1_text
      , "was recently pushed. Waiting for builds..."
      ]


-- | whether to store second-level build records for "success" status
data SuccessRecordStorageMode =
    ShouldStoreDetailedSuccessRecords
  | NoStoreDetailedSuccessRecords


data ScanLogsMode =
    ShouldScanLogs
  | NoScanLogs


-- | Operations:
-- * Filter out the CircleCI builds
-- * Check if the builds have been scanned yet.
-- * Scan each CircleCI build that needs to be scanned.
-- * For each match, check if that match's pattern is tagged as "flaky".
readGitHubStatusesAndScanAndPostSummaryForCommit ::
     CircleApi.ThirdPartyAuth
  -> Connection
  -> Maybe AuthStages.Username -- ^ scan initiator
  -> DbHelpers.OwnerAndRepo
  -> SuccessRecordStorageMode
  -> Builds.RawCommit
  -> ScanLogsMode
  -> Scanning.RevisitationMode
  -> ExceptT LT.Text IO ()
readGitHubStatusesAndScanAndPostSummaryForCommit
    third_party_auth
    conn
    maybe_initiator
    owned_repo
    should_store_second_level_success_records
    sha1
    should_scan
    should_revisit_scanned = do

  liftIO $ do
    current_time <- Clock.getCurrentTime
    D.debugList ["ABC Processing at", show current_time]

  liftIO $ DbHelpers.setSessionStatementTimeout conn Scanning.scanningStatementTimeoutSeconds

  scan_resources <- ExceptT $ first LT.fromStrict <$>
    Scanning.prepareScanResources
      third_party_auth
      conn
      Scanning.PersistScanResult
      maybe_initiator

  (scannable_build_numbers, _circleci_failcount) <- getBuildsFromGithub
    (ScanRecords.fetching scan_resources)
    owned_repo
    should_store_second_level_success_records
    sha1

  liftIO $ D.debugList ["ABC Finished getBuildsFromGithub"]

  case should_scan of
    ShouldScanLogs -> do
      liftIO $ D.debugList ["ABC About to enter scanAndPost"]

      scanAndPost
        scan_resources
        should_revisit_scanned
        scannable_build_numbers
        owned_repo
        sha1
    NoScanLogs -> return ()


handlePullRequestWebhook ::
     DbHelpers.DbConnectionData
  -> CircleApi.ThirdPartyAuth
  -> PullRequestWebhooks.GitHubPullRequestEvent
  -> IO (Either LT.Text Int64)
handlePullRequestWebhook
    db_connection_data
    third_party_auth
    pr_event = do

  D.debugList [
      "Got PR event for PR number"
    , show pr_num
    , "at head:"
    , show new_pr_head_commit
    , "for action:"
    , LT.unpack actn
    ]

  insertion_count <- if pullRequestEventActionSynchronize == actn
    then do
      synchronous_conn <- DbHelpers.getConnection db_connection_data

      (maybe_old_pr_comment, pr_heads_insertion_count) <- flip runReaderT synchronous_conn $ do
        a1 <- SqlRead.getPostedCommentForPR pr_number

        a2 <- SqlWrite.insertPullRequestHeads True [(pr_number, new_pr_head_commit)]
        return (a1, a2)

      case maybe_old_pr_comment of
        Nothing -> return $ Right ()
        Just previous_comment@(SqlRead.PostedPRComment _ _ _ comment_sha1 _ _ _ _) ->
          if comment_sha1 /= new_pr_head_commit
          then runExceptT $ do

            let rsa_signer = CircleApi.jwt_signer third_party_auth
            github_auth_token <- ExceptT $ first (LT.fromStrict . T.pack) <$>
              CircleAuth.getGitHubAppInstallationToken rsa_signer

            let access_token = CircleAuth.token github_auth_token

            wipeCommentForUpdatedPr
              access_token
              Constants.pytorchOwnedRepo
              synchronous_conn
              previous_comment
              pr_number
              new_pr_head_commit

           else return $ pure ()

      return pr_heads_insertion_count

    else return 0

  return $ Right insertion_count

  where
    new_pr_head_commit = PullRequestWebhooks.sha $ PullRequestWebhooks.head pr_obj
    PullRequestWebhooks.GitHubPullRequestEvent actn pr_number@(Builds.PullRequestNumber pr_num) pr_obj = pr_event

handlePushWebhook ::
     DbHelpers.DbConnectionData
  -> CircleApi.ThirdPartyAuth
  -> PushWebhooks.GitHubPushEvent
  -> IO (Either LT.Text (Int64, Int64))
handlePushWebhook
    db_connection_data
    third_party_auth
    push_event = do

  D.debugList [
      "Got repo push event for ref"
    , T.unpack refname
    , "at head:"
    , head_sha1
    ]

  if refname == fullMasterRefName
    then do
      putStrLn "This was the master branch!"
      conn <- DbHelpers.getConnection db_connection_data

      fmap (first LT.fromStrict) $ runExceptT $ do

        access_token_container <- ExceptT $ fmap (first T.pack) $
          CircleAuth.getGitHubAppInstallationToken $ CircleApi.jwt_signer third_party_auth

        let access_token = CircleAuth.token access_token_container

        ExceptT $ SqlWrite.populateLatestMasterCommits
          conn
          access_token
          owned_repo
  else
    return $ Right mempty

  where
    refname = LT.toStrict $ PushWebhooks.ref push_event

    head_sha1 = LT.unpack $ PushWebhooks.id $ PushWebhooks.head_commit push_event
    repo_object = PushWebhooks.repository push_event
    owned_repo = DbHelpers.OwnerAndRepo
      (LT.unpack $ PushWebhooks.organization repo_object)
      (LT.unpack $ PushWebhooks.name repo_object)


handleStatusWebhook ::
     DbHelpers.DbConnectionData
  -> CircleApi.ThirdPartyAuth
  -> Webhooks.GitHubStatusEvent
  -> IO (Either LT.Text Bool)
handleStatusWebhook
    db_connection_data
    _third_party_auth
    status_event = do

  liftIO $ D.debugList [
      "Notified status context was:"
    , notified_status_context_string
    ]

  let notified_status_url_string = LT.unpack $ Webhooks.target_url status_event
  when (circleCIContextPrefix `T.isPrefixOf` notified_status_context_text) $
    liftIO $ D.debugList [
        "CircleCI URL was:"
      , notified_status_url_string
      ]

  {-
  let owner_repo_text = Webhooks.name status_event
      splitted = splitOn "/" $ LT.unpack owner_repo_text
  -}
  runExceptT $ do

    {-
    owned_repo <- except $ case splitted of
      [org, repo] -> Right $ DbHelpers.OwnerAndRepo org repo
      _ -> Left $ "un-parseable owner/repo text: " <> owner_repo_text
    -}

    synchronous_conn <- liftIO $ DbHelpers.getConnection db_connection_data

    liftIO $ SqlWrite.insertReceivedGithubStatus synchronous_conn status_event

    {-
    wrappedScanAndPostCommit
      db_connection_data
      third_party_auth
      synchronous_conn
      owned_repo
      is_failure_notification
      sha1
     -}
  return $ return False

  where
    {-
    notified_status_state_string = LT.unpack $ Webhooks.state status_event
    is_failure_notification = notified_status_state_string == LT.unpack gitHubStatusFailureString
    -}

    context_text = Webhooks.context status_event
    notified_status_context_string = LT.unpack context_text
    notified_status_context_text = LT.toStrict context_text

--    sha1 = Builds.RawCommit $ LT.toStrict $ Webhooks.sha status_event


wrappedScanAndPostCommit ::
     Connection
  -> CircleApi.ThirdPartyAuth
  -> DbHelpers.OwnerAndRepo
  -> Bool
  -> Builds.RawCommit
  -> IO Bool
wrappedScanAndPostCommit
    conn
    third_party_auth
    owned_repo
    is_failure_notification
    sha1 = do

  -- On builds from the *master* branch,
  -- we may store the *successful* as well as the failed second-level
  -- build records,
  -- since the volume on the *master* branch should be relatively low.
  is_master_commit <- liftIO $ flip runReaderT conn $ SqlRead.isMasterCommit sha1
  let success_storage_mode = if is_master_commit
        then StatusUpdate.ShouldStoreDetailedSuccessRecords
        else StatusUpdate.NoStoreDetailedSuccessRecords

  maybe_previously_posted_status <- liftIO $ flip runReaderT conn $
    SqlRead.getPostedCommentForSha1 sha1

  -- If we haven't posted a PR comment before for this commit, do not act unless the notification
  -- was for a failed build.
  let will_post = is_failure_notification || not (null maybe_previously_posted_status)

  let dr_ci_posting_computation = do

        runExceptT $
          -- When we receive a webhook notification of a "status" event from
          -- GitHub, and that status was "failure", we take a look at all of
          -- the statuses for that commit, scan the build logs, and post
          -- post a summary as a GitHub status notification.
          readGitHubStatusesAndScanAndPostSummaryForCommit
            third_party_auth
            conn
            Nothing
            owned_repo
            success_storage_mode
            sha1
            ShouldScanLogs
            Scanning.NoRevisit

        return ()

  when will_post $ do
    dr_ci_posting_computation
    return ()

  return will_post


githubEventEndpoint ::
     Constants.ProviderConfigs
  -> ActionT LT.Text IO ()
githubEventEndpoint
    (Constants.ProviderConfigs github_config third_party_auth connection_data) = do

  maybe_signature_header <- S.header "X-Hub-Signature"
  rq_body <- S.body

  let is_signature_valid = GHValidate.isValidPayload
        (AuthConfig.webhook_secret github_config)
        (LT.toStrict <$> maybe_signature_header)
        (LBS.toStrict rq_body)

  maybe_event_type <- S.header "X-GitHub-Event"

  current_time <- liftIO Clock.getCurrentTime

  case maybe_event_type of
    Nothing -> return ()
    Just event_type -> when is_signature_valid $
      case event_type of
        "status" -> do
          body_json <- S.jsonData

          will_post <- liftIO $ do

            D.debugList [
                "Parsed 'status' event body JSON at"
              , show current_time
              ]

            handleStatusWebhook
              connection_data
              third_party_auth
              body_json

          S.json =<< return ["Will post?" :: String, show will_post]


        "pull_request" -> do
          body_json <- S.jsonData

          liftIO $ do
            D.debugList [
                "Parsed 'pull_request' event body JSON at"
              , show current_time
              ]

            handlePullRequestWebhook
              connection_data
              third_party_auth
              body_json

            D.debugStr "Handled pull_request event."
          S.json =<< return ["hello" :: String]

        "push" -> do
          body_json <- S.jsonData

          liftIO $ do
            D.debugList [
                "Parsed 'push' event body JSON at"
              , show current_time
              ]

            handlePushWebhook
              connection_data
              third_party_auth
              body_json

            D.debugStr "Handled push event."
          S.json =<< return ["hello" :: String]

        _ -> return ()

