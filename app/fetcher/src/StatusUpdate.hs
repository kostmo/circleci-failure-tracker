{-# LANGUAGE OverloadedStrings #-}

module StatusUpdate (
    githubEventEndpoint
  , readGitHubStatusesAndScanAndPostSummaryForCommit
  , getBuildsFromGithub
  , postCommitSummaryStatus
  ) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (guard, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except    (ExceptT (ExceptT), except,
                                                runExceptT)
import           Control.Monad.Trans.Reader    (runReaderT)
import           Data.Bifunctor                (first)
import qualified Data.ByteString.Lazy          as LBS
import           Data.Foldable                 (for_)
import           Data.List                     (filter, intercalate)
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
import           System.Timeout                (timeout)
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S
import           Web.Scotty.Internal.Types     (ActionT)

import qualified ApiPost
import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified DbHelpers
import qualified GadgitFetch
import qualified GithubApiFetch
import qualified Markdown
import qualified MyUtils
import qualified PushWebhooks
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusEvent
import qualified StatusEventQuery
import qualified Webhooks


-- | 3 minutes
buildStatusHandlerTimeoutMicroseconds :: Int
buildStatusHandlerTimeoutMicroseconds = 1000000 * 60 * 3


-- | This is short so we don't
-- clog up the database with backed up requests
scanningStatementTimeoutSeconds :: Integer
scanningStatementTimeoutSeconds = 20


fullMasterRefName :: Text
fullMasterRefName = "refs/heads/" <> Builds.masterName


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://dr.pytorch.org"


circleciDomain :: String
circleciDomain = "circleci.com"


circleCIContextPrefix :: Text
circleCIContextPrefix = "ci/circleci: "


-- | Name is "Dr. CI" -- where doctor alludes to "diagnostician".
-- It is prefixed with an undescore so it appears first in the lexicographical
-- ordering in the faild builds list.
myAppStatusContext :: Text
myAppStatusContext = "_dr.ci"


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
storeUniversalBuilds ::
     Connection
  -> Builds.RawCommit
  -> [([StatusEventQuery.GitHubStatusEventGetter], DbHelpers.WithId String)]
  -> IO [(Builds.StorableBuild, (StatusEventQuery.GitHubStatusEventGetter, String))]
storeUniversalBuilds conn commit statuses_by_ci_providers = do

  result_lists <- for statuses_by_ci_providers $ \(statuses, provider_with_id) -> do

    result_maybe_list <- for statuses $ \status_event -> do

      let maybe_universal_build = extractUniversalBuild
            commit
            provider_with_id
            status_event

      case maybe_universal_build of
        Nothing -> return Nothing
        Just (sub_build, uni_build) -> do
          stored_uni_build <- SqlWrite.insertSingleUniversalBuild conn uni_build
          return $ Just (Builds.StorableBuild stored_uni_build sub_build, (status_event, DbHelpers.record provider_with_id))

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
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Bool
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ([Builds.UniversalBuildId], Int)
getBuildsFromGithub
    conn
    access_token
    owned_repo
    store_provider_specific_success_records
    sha1  = do

  build_statuses_list_any_source <- ExceptT $ GithubApiFetch.getBuildStatuses
    access_token
    owned_repo
    sha1

  liftIO $ MyUtils.debugList [
      "Build statuses count:"
    , show $ length build_statuses_list_any_source
    ]

  let statuses_list_not_mine = filter is_not_my_own_context build_statuses_list_any_source

      succeeded_or_failed_statuses = filter ((`elem` conclusiveStatuses) . StatusEventQuery._state) statuses_list_not_mine

      statuses_by_hostname = groupStatusesByHostname succeeded_or_failed_statuses

  statuses_by_ci_providers <- liftIO $ SqlWrite.getAndStoreCIProviders conn statuses_by_hostname


  -- Only store succeeded or failed builds; ignore pending or aborted
  stored_build_tuples <- liftIO $ storeUniversalBuilds
    conn
    sha1
    statuses_by_ci_providers

  let circleci_builds_and_statuses = filter ((== circleciDomain) . snd . snd) stored_build_tuples

      filter_by_status stat = filter $ (== stat) . StatusEventQuery._state . fst . snd

      circleci_failed_builds = map fst $ filter_by_status gitHubStatusFailureString circleci_builds_and_statuses

      -- TODO: May want to use the CircleCI API to retrieve more info
      -- on these builds.
      -- Currently, the extra fields available from CircleCI
      -- are only populated through the "BuildRetrieval.updateCircleCIBuildsList"
      -- function, which is manually invoked.
      circleci_successful_builds = map fst $ filter_by_status gitHubStatusSuccessString circleci_builds_and_statuses

      second_level_storable_builds = if store_provider_specific_success_records
        then circleci_failed_builds ++ circleci_successful_builds
        else circleci_failed_builds

      scannable_build_numbers = map (Builds.UniversalBuildId . DbHelpers.db_id . Builds.universal_build) circleci_failed_builds

      circleci_failcount = length circleci_failed_builds

  liftIO $ MyUtils.debugList [
      "Failed CircleCI build count:"
    , show circleci_failcount
    ]


  liftIO $ SqlWrite.storeBuildsList conn Nothing $
    map storable_build_to_universal second_level_storable_builds

  return (scannable_build_numbers, circleci_failcount)

  where
    storable_build_to_universal (Builds.StorableBuild (DbHelpers.WithId ubuild_id _ubuild) rbuild) =
      DbHelpers.WithTypedId (Builds.UniversalBuildId ubuild_id) rbuild

    is_not_my_own_context = (/= myAppStatusContext) . LT.toStrict . StatusEventQuery._context


scanAndPost ::
     Connection
  -> OAuth2.AccessToken
  -> Maybe AuthStages.Username -- ^ scan initiator
  -> [Builds.UniversalBuildId]
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> ExceptT LT.Text IO ()
scanAndPost
    conn
    access_token
    maybe_initiator
    scannable_build_numbers
    owned_repo
    sha1 = do

  scan_matches <- liftIO $ do

    scan_resources <- Scanning.prepareScanResources conn maybe_initiator
    DbHelpers.setSessionStatementTimeout conn scanningStatementTimeoutSeconds

    Scanning.scanBuilds
      scan_resources
      False -- do not attempt to revisit previously scanned builds for new patterns
      False -- do not re-download log
      (Left $ Set.fromList scannable_build_numbers)

  postCommitSummaryStatus
    conn
    access_token
    owned_repo
    sha1
    scan_matches


postCommitSummaryStatus ::
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> [(a, [ScanPatterns.ScanMatch])]
  -> ExceptT LT.Text IO ()
postCommitSummaryStatus
    conn
    access_token
    owned_repo
    sha1
    scan_matches = do

  basic_revision_stats <- ExceptT $ runReaderT (SqlRead.getNonPatternMatchRevisionStats sha1) conn

  let (SqlRead.BasicRevisionBuildStats _ _ _ _ _ circleci_failcount) = basic_revision_stats



  -- TODO WIP!!!
  (_manually_annotated_breakages, inferred_upstream_caused_broken_jobs) <- ExceptT $
    first LT.fromStrict <$> SqlUpdate.findKnownBuildBreakages
      conn
      access_token
      owned_repo
      sha1

  let known_broken_circle_builds = inferred_upstream_caused_broken_jobs
--      circleci_failed_builds = []  -- FIXME TODO!!!
--    all_broken_jobs = Set.unions $ map (SqlRead._jobs . DbHelpers.record) manually_annotated_breakages
--    known_broken_circle_builds = filter ((`Set.member` all_broken_jobs) . Builds.job_name . Builds.build_record) circleci_failed_builds
      known_broken_circle_build_count = length known_broken_circle_builds


  postCommitSummaryStatusInner
    circleci_failcount
    known_broken_circle_build_count
    conn
    access_token
    owned_repo
    sha1
    scan_matches


data BuildSummaryStats = BuildSummaryStats {
    flaky_count       :: Int -- ^ flaky count
  , _pre_broken_count :: Int -- ^ pre-broken count
  , total_failcount   :: Int -- ^ total failure count
  }


postCommitSummaryStatusInner
    circleci_failcount
    known_broken_circle_build_count
    conn
    access_token
    owned_repo
    sha1
    scan_matches = do

  maybe_previously_posted_status <- liftIO $
    runReaderT (SqlRead.getPostedGithubStatus owned_repo sha1) conn

  case maybe_previously_posted_status of
    Nothing -> when (circleci_failcount > 0) post_and_store
    Just previous_state_description_tuple ->
      when (previous_state_description_tuple /= new_state_description_tuple)
        post_and_store

  post_pr_comment_and_store

  where
  build_summary_stats = BuildSummaryStats flaky_count known_broken_circle_build_count circleci_failcount

  -- TODO - we should instead see if the "best matching pattern" is
  -- flaky, rather than checking if *any* matching pattern is a
  -- "flaky" pattern.
  -- See Issue #63
  flaky_predicate = any (ScanPatterns.is_flaky . DbHelpers.record . ScanPatterns.scanned_pattern) . snd
  builds_with_flaky_pattern_matches = filter flaky_predicate scan_matches

  flaky_count = length builds_with_flaky_pattern_matches
  status_setter_data = genFlakinessStatus sha1 build_summary_stats


  new_state_description_tuple = (
      LT.toStrict $ StatusEvent._state status_setter_data
    , LT.toStrict $ StatusEvent._description status_setter_data
    )

  -- We're examining statuses on both failed and successful build notifications, which can add
  -- up to a lot of activity.
  -- We only should re-post our summary status if it will change what was already posted,
  -- since we don't want GitHub to throttle our requests.

  post_and_store = do
    post_result <- ExceptT $ ApiPost.postCommitStatus
      access_token
      owned_repo
      sha1
      status_setter_data

    liftIO $ SqlWrite.insertPostedGithubStatus
      conn
      sha1
      owned_repo
      post_result

    return ()



  post_initial_comment pr_number = do
    comment_post_result <- ExceptT $ ApiPost.postPullRequestComment
      access_token
      owned_repo
      pr_number
      pr_comment_text

    liftIO $ putStrLn "Here F"

    liftIO $ SqlWrite.insertPostedGithubComment
      conn
      owned_repo
      pr_number
      comment_post_result
    where
      pr_comment_text = generateCommentMarkdown Nothing build_summary_stats sha1


  update_comment_or_fallback pr_number previous_pr_comment = do
    liftIO $ putStrLn "Here G"
    either_comment_update_result <- liftIO $ ApiPost.updatePullRequestComment
      access_token
      owned_repo
      comment_id
      pr_comment_text

    liftIO $ putStrLn "Here H"

    case either_comment_update_result of
      Right comment_update_result ->
        liftIO $ SqlWrite.modifyPostedGithubComment
          conn
          comment_update_result

      -- If the comment was deleted, we need to re-post one.
      Left "Not Found" -> do
        liftIO $ MyUtils.debugStr "Comment was deleted. Posting a new one..."

        -- Mark our database entry as stale
        liftIO $ SqlWrite.markPostedGithubCommentAsDeleted conn comment_id

        post_initial_comment pr_number

      Left other_failure_message -> except $ Left other_failure_message

    where
      pr_comment_text = generateCommentMarkdown (Just previous_pr_comment) build_summary_stats sha1
      comment_id =  ApiPost.CommentId $ SqlRead._comment_id previous_pr_comment


  post_pr_comment_and_store = do
    liftIO $ putStrLn "Here A"
    containing_pr_list <- ExceptT $ first LT.pack <$> GadgitFetch.getContainingPRs sha1

    liftIO $ putStrLn "Here B"
    for_ containing_pr_list $ \pr_number -> do

      liftIO $ putStrLn "Here C"
      maybe_previous_pr_comment <- liftIO $ runReaderT (SqlRead.getPostedCommentForPR pr_number) conn

      liftIO $ putStrLn "Here D"
      case maybe_previous_pr_comment of
        Nothing -> do
          liftIO $ putStrLn "Here E"
          post_initial_comment pr_number

        Just previous_pr_comment -> update_comment_or_fallback pr_number previous_pr_comment

      liftIO $ putStrLn "Here I"


generateCommentMarkdown
    maybe_previous_pr_comment
    build_summary_stats
    (Builds.RawCommit sha1_text) =
  Markdown.paragraphs $ preliminary_lines_list ++ optional_suffix
  where
    preliminary_lines_list = [
        T.unlines [
          Markdown.heading 2 "Build failures summary"
        , "As of commit " <> T.take 7 sha1_text <> ":"
        , Markdown.bullets $ map T.pack $ genMetricsList build_summary_stats
        ]
      , Markdown.sentence [
          "Here are the"
        , Markdown.link "reasons each build failed" dr_ci_commit_details_link
        ]
      , T.unlines [
          Markdown.sentence [
            "This comment was automatically generated by"
          , Markdown.link "Dr. CI" dr_ci_base_url
          ]
        , Markdown.sentence [
            "Follow"
          , Markdown.link "this link to opt-out" "https://dr.pytorch.org/admin/comments-opt-out.html"
          , "for your Pull Requests"
          ]
        ]
      ]

    -- Note that the revision count will be accurate for the (N+1)th comment using the current count
    -- of N comments, because the first post doesn't count as a "revision".
    optional_suffix = case maybe_previous_pr_comment of
      Nothing -> []
      Just previous_pr_comment -> [
          Markdown.italic $ Markdown.sentence [
            "This comment has been revised"
          , T.pack $ show $ SqlRead._revision_count previous_pr_comment
          , "time(s)"
          ]
        ]

    dr_ci_base_url = LT.toStrict webserverBaseUrl
    dr_ci_commit_details_link = dr_ci_base_url <> "/commit-details.html?sha1=" <> sha1_text


-- | Operations:
-- * Filter out the CircleCI builds
-- * Check if the builds have been scanned yet.
-- * Scan each CircleCI build that needs to be scanned.
-- * For each match, check if that match's pattern is tagged as "flaky".
readGitHubStatusesAndScanAndPostSummaryForCommit ::
     Connection
  -> OAuth2.AccessToken
  -> Maybe AuthStages.Username -- ^ scan initiator
  -> DbHelpers.OwnerAndRepo
  -> Bool -- ^ should store second-level build records for "success" status
  -> Builds.RawCommit
  -> Bool
  -> ExceptT LT.Text IO ()
readGitHubStatusesAndScanAndPostSummaryForCommit
    conn
    access_token
    maybe_initiator
    owned_repo
    should_store_second_level_success_records
    sha1
    should_scan = do

  liftIO $ do
    current_time <- Clock.getCurrentTime
    MyUtils.debugList ["Processing at", show current_time]

  (scannable_build_numbers, _circleci_failcount) <- getBuildsFromGithub
      conn
      access_token
      owned_repo
      should_store_second_level_success_records
      sha1

  when should_scan $
    scanAndPost
      conn
      access_token
      maybe_initiator
      scannable_build_numbers
      owned_repo
      sha1


handlePushWebhook ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> PushWebhooks.GitHubPushEvent
  -> IO (Either LT.Text (Int64, Int64))
handlePushWebhook
    db_connection_data
    access_token
    push_event = do

  MyUtils.debugList [
      "Got repo push event for ref"
    , T.unpack refname
    , "at head:"
    , head_sha1
    ]

  if refname == fullMasterRefName
    then do
      putStrLn "This was the master branch!"
      conn <- DbHelpers.get_connection db_connection_data

      first LT.fromStrict <$> SqlWrite.populateLatestMasterCommits
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
  -> OAuth2.AccessToken
  -> Maybe AuthStages.Username
  -> Webhooks.GitHubStatusEvent
  -> IO (Either LT.Text Bool)
handleStatusWebhook
    db_connection_data
    access_token
    maybe_initiator
    status_event = do

  liftIO $ MyUtils.debugList [
      "Notified status context was:"
    , notified_status_context_string
    ]

  let notified_status_url_string = LT.unpack $ Webhooks.target_url status_event
  when (circleCIContextPrefix `T.isPrefixOf` notified_status_context_text) $
    liftIO $ MyUtils.debugList [
        "CircleCI URL was:"
      , notified_status_url_string
      ]

  let owner_repo_text = Webhooks.name status_event
      splitted = splitOn "/" $ LT.unpack owner_repo_text

  runExceptT $ do

    owned_repo <- except $ case splitted of
      [org, repo] -> Right $ DbHelpers.OwnerAndRepo org repo
      _ -> Left $ "un-parseable owner/repo text: " <> owner_repo_text


    synchronous_conn <- liftIO $ DbHelpers.get_connection db_connection_data

    liftIO $ SqlWrite.insertReceivedGithubStatus synchronous_conn status_event

    maybe_previously_posted_status <- liftIO $
      runReaderT (SqlRead.getPostedGithubStatus owned_repo sha1) synchronous_conn


    -- On builds from the *master* branch,
    -- we may store the *successful* as well as the failed second-level
    -- build records,
    -- since the volume on the *master* branch should be relatively low.
    is_master_commit <- liftIO $
      runReaderT (SqlRead.isMasterCommit sha1) synchronous_conn



    let dr_ci_posting_computation = do
          conn <- DbHelpers.get_connection db_connection_data

          timeout buildStatusHandlerTimeoutMicroseconds $ runExceptT $
            -- When we receive a webhook notification of a "status" event from
            -- GitHub, and that status was "failure", we take a look at all of
            -- the statuses for that commit, scan the build logs, and post
            -- post a summary as a GitHub status notification.
            readGitHubStatusesAndScanAndPostSummaryForCommit
              conn
              access_token
              maybe_initiator
              owned_repo
              is_master_commit
              sha1
              -- FIXME Disabled for now due to load issues
              False

          return ()


    -- Do not act on receipt of statuses from the context I have created, or else
    -- we may get stuck in an infinite notification loop
    --
    -- Also, if we haven't posted a summary status before, do not act unless the notification
    -- was for a failed build.
    let will_post = is_not_my_own_context && (is_failure_notification || not (null maybe_previously_posted_status))

    when will_post $ do
      _thread_id <- liftIO $ forkIO dr_ci_posting_computation
      return ()


    return will_post

  where
    notified_status_state_string = LT.unpack $ Webhooks.state status_event
    is_failure_notification = notified_status_state_string == LT.unpack gitHubStatusFailureString

    context_text = Webhooks.context status_event
    notified_status_context_string = LT.unpack context_text
    notified_status_context_text = LT.toStrict context_text

    is_not_my_own_context = notified_status_context_text /= myAppStatusContext
    sha1 = Builds.RawCommit $ LT.toStrict $ Webhooks.sha status_event


genMetricsList (BuildSummaryStats flaky_count pre_broken_count total_failcount) = [
    show flaky_count <> "/" <> show total_failcount <> " flaky"
  ] ++ optional_kb_metric
  where
    optional_kb_metric = if pre_broken_count > 0
      then [show pre_broken_count <> "/" <> show total_failcount <> " broken upstream"]
      else []


genFlakinessStatus ::
     Builds.RawCommit
  -> BuildSummaryStats
  -> StatusEvent.GitHubStatusEventSetter
genFlakinessStatus (Builds.RawCommit sha1) build_summary_stats =

  StatusEvent.GitHubStatusEventSetter
    description
    status_string
    (webserverBaseUrl <> "/commit-details.html?sha1=" <> LT.fromStrict sha1)
    (LT.fromStrict myAppStatusContext)

  where
    description = LT.pack $ unwords [
--        "(deprecated)"
        "(experimental)"
      , intercalate ", " $ genMetricsList build_summary_stats
      ]

    status_string = if flaky_count build_summary_stats == total_failcount build_summary_stats
      then gitHubStatusSuccessString
      else gitHubStatusFailureString


githubEventEndpoint ::
     DbHelpers.DbConnectionData
  -> AuthConfig.GithubConfig
  -> ActionT LT.Text IO ()
githubEventEndpoint connection_data github_config = do

  maybe_signature_header <- S.header "X-Hub-Signature"
  rq_body <- S.body

  let is_signature_valid = GHValidate.isValidPayload
        (AuthConfig.webhook_secret github_config)
        (LT.toStrict <$> maybe_signature_header)
        (LBS.toStrict rq_body)

  maybe_event_type <- S.header "X-GitHub-Event"

  case maybe_event_type of
    Nothing -> return ()
    Just event_type -> when is_signature_valid $
      case event_type of
        "status" -> do
          body_json <- S.jsonData

          will_post <- liftIO $ handleStatusWebhook
            connection_data
            (AuthConfig.personal_access_token github_config)
            Nothing
            body_json

          S.json =<< return ["Will post?" :: String, show will_post]

        "push" -> do
          body_json <- S.jsonData

          liftIO $ do
            putStrLn "Parsed push event body JSON..."
            handlePushWebhook connection_data (AuthConfig.personal_access_token github_config) body_json
            putStrLn "Handled push event."
          S.json =<< return ["hello" :: String]

        _ -> return ()
