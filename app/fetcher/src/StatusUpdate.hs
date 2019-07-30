{-# LANGUAGE OverloadedStrings #-}

module StatusUpdate (
    githubEventEndpoint
  , handleFailedStatuses
  ) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (guard, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except    (ExceptT (ExceptT), except,
                                                runExceptT)
import           Data.Bifunctor                (first)
import qualified Data.ByteString.Lazy          as LBS
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
import qualified DbHelpers
import qualified GithubApiFetch
import qualified MyUtils
import qualified PushWebhooks
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified StatusEvent
import qualified StatusEventQuery
import qualified Webhooks


fullMasterRefName :: Text
fullMasterRefName = "refs/heads/" <> Builds.masterName


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://circle.pytorch.org"


circleciDomain :: String
circleciDomain = "circleci.com"


circleCIContextPrefix :: Text
circleCIContextPrefix = "ci/circleci: "


-- | Name is "Dr. CI" -- where doctor alludes to "diagnostician".
-- It is prefixed with an undescore so it appears first in the lexicographical
-- ordering in the faild builds list.
myAppStatusContext :: Text
myAppStatusContext = "_dr.ci"


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
    ""

  where
    context_text = LT.toStrict context
    build_name = T.drop (T.length circleCIContextPrefix) context_text

    current_time = StatusEventQuery._created_at event_setter
    context = StatusEventQuery._context event_setter
    url_text = StatusEventQuery._target_url event_setter


-- | TODO return Left for each universal build that violated its uniqueness constraint
storeUniversalBuilds ::
     DbHelpers.DbConnectionData
  -> Builds.RawCommit
  -> [([StatusEventQuery.GitHubStatusEventGetter], DbHelpers.WithId String)]
  -> IO [(Builds.StorableBuild, (StatusEventQuery.GitHubStatusEventGetter, String))]
storeUniversalBuilds conn_data commit statuses_by_ci_providers = do
  conn <- DbHelpers.get_connection conn_data

  result_lists <- for statuses_by_ci_providers $ \(statuses, provider_with_id) -> do

    result_maybe_list <- for statuses $ \status_event -> do

      let maybe_universal_build = extractUniversalBuild commit provider_with_id status_event
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
    did_succeed = StatusEventQuery._state status_object == "success"


-- | Operations:
-- * Filter out the CircleCI builds
-- * Check if the builds have been scanned yet.
-- * Scan each CircleCI build that needs to be scanned.
-- * For each match, check if that match's pattern is tagged as "flaky".
handleFailedStatuses ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> Maybe AuthStages.Username -- ^ scan initiator
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> Maybe (Text, Text)
  -> ExceptT LT.Text IO ()
handleFailedStatuses
    db_connection_data
    access_token
    maybe_initiator
    owned_repo
    sha1
    maybe_previously_posted_status = do

  liftIO $ do
    current_time <- Clock.getCurrentTime
    putStrLn $ "Processing at " ++ show current_time

  build_statuses_list_any_source <- ExceptT $ GithubApiFetch.getBuildStatuses access_token owned_repo sha1

  liftIO $ putStrLn $ "Build statuses count: " ++ show (length build_statuses_list_any_source)

  let statuses_list_not_mine = filter is_not_my_own_context build_statuses_list_any_source

      succeeded_or_failed_statuses = filter ((`elem` ["failure", "success"]) . StatusEventQuery._state) statuses_list_not_mine

      statuses_by_hostname = groupStatusesByHostname succeeded_or_failed_statuses

  statuses_by_ci_providers <- liftIO $ SqlWrite.getAndStoreCIProviders db_connection_data statuses_by_hostname

  -- debug info:
  let provider_keys = map (DbHelpers.db_id . snd) statuses_by_ci_providers
  liftIO $ putStrLn $ unwords ["Provider DB keys:", show provider_keys]

  -- Only store succeeded or failed builds; ignore pending or aborted
  stored_build_tuples <- liftIO $ storeUniversalBuilds
    db_connection_data
    sha1
    statuses_by_ci_providers

  let circleci_builds_and_statuses = filter ((== circleciDomain) . snd . snd) stored_build_tuples
      filter_failed = filter $ (== "failure") . StatusEventQuery._state . fst . snd

      circleci_failed_builds_and_statuses = filter_failed circleci_builds_and_statuses

      circleci_failed_builds = map fst circleci_failed_builds_and_statuses
      scannable_build_numbers = map (Builds.UniversalBuildId . DbHelpers.db_id . Builds.universal_build) circleci_failed_builds

      circleci_failcount = length circleci_failed_builds

  liftIO $ putStrLn $ "Failed CircleCI build count: " ++ show circleci_failcount

  -- XXX TEMPORARILY DISABLED FOR PERFORMANCE REASONS
  let known_breakages = []
--  known_breakages <- ExceptT $ do
--    conn <- liftIO $ DbHelpers.get_connection db_connection_data
--    first LT.fromStrict <$> SqlUpdate.findKnownBuildBreakages conn access_token owned_repo (Builds.RawCommit sha1)

  let all_broken_jobs = Set.unions $ map (SqlRead._jobs . DbHelpers.record) known_breakages
      known_broken_circle_builds = filter ((`Set.member` all_broken_jobs) . Builds.job_name . Builds.build_record) circleci_failed_builds
      known_broken_circle_build_count = length known_broken_circle_builds


  builds_with_flaky_pattern_matches <- liftIO $ do
    conn <- DbHelpers.get_connection db_connection_data
    scan_resources <- Scanning.prepareScanResources conn maybe_initiator
    SqlWrite.storeBuildsList conn circleci_failed_builds
    scan_matches <- Scanning.scanBuilds scan_resources True $ Left $ Set.fromList scannable_build_numbers

    -- TODO - we should instead see if the "best matching pattern" is
    -- flaky, rather than checking if *any* matching pattern is a
    -- "flaky" pattern.
    flaky_pattern_ids <- SqlRead.get_flaky_pattern_ids conn
    let flaky_predicate = any ((`Set.member` flaky_pattern_ids) . DbHelpers.db_id . ScanPatterns.scanned_pattern) . snd
        builds_with_flaky_pattern_matches = filter flaky_predicate scan_matches
    return builds_with_flaky_pattern_matches

  let flaky_count = length builds_with_flaky_pattern_matches
      status_setter_data = genFlakinessStatus sha1 flaky_count known_broken_circle_build_count circleci_failcount

      new_state_description_tuple = (LT.toStrict $ StatusEvent._state status_setter_data, LT.toStrict $ StatusEvent._description status_setter_data)

  -- We're examining statuses on both failed and successful build notifications, which can add
  -- up to a lot of activity.
  -- We only should re-post our summary status if it will change what was already posted,
  -- since we don't want GitHub to throttle our requests.

  let post_and_store = do
        post_result <- ExceptT $ ApiPost.postCommitStatus
          access_token
          owned_repo
          sha1
          status_setter_data

        liftIO $ SqlWrite.insert_posted_github_status db_connection_data sha1 owned_repo post_result
        return ()

  case maybe_previously_posted_status of
    Nothing -> when (circleci_failcount > 0) post_and_store
    Just previous_state_description_tuple ->
      when (previous_state_description_tuple /= new_state_description_tuple)
        post_and_store

  where
    is_not_my_own_context = (/= myAppStatusContext) . LT.toStrict . StatusEventQuery._context


handlePushWebhook ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> PushWebhooks.GitHubPushEvent
  -> IO (Either LT.Text ())
handlePushWebhook
    db_connection_data
    access_token
    push_event = do

  putStrLn $ unwords [
      "Got repo push event for ref"
    , T.unpack refname
    , "at head:"
    , head_sha1
    ]

  if refname == fullMasterRefName
    then do
      putStrLn "This was the master branch!"
      conn <- DbHelpers.get_connection db_connection_data

      -- FIXME the Either is just being absorbed here
      first LT.fromStrict <$> SqlWrite.populateLatestMasterCommits
        conn
        access_token
        owned_repo

      return $ Right ()
  else
    return $ Right ()

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

  liftIO $ putStrLn $ "Notified status context was: " ++ notified_status_context_string

  let notified_status_url_string = LT.unpack $ Webhooks.target_url status_event
  when (circleCIContextPrefix `T.isPrefixOf` notified_status_context_text) $
    liftIO $ putStrLn $ "CircleCI URL was: " ++ notified_status_url_string

  let owner_repo_text = Webhooks.name status_event
      splitted = splitOn "/" $ LT.unpack owner_repo_text

  runExceptT $ do

    owned_repo <- except $ case splitted of
      [org, repo] -> Right $ DbHelpers.OwnerAndRepo org repo
      _ -> Left $ "un-parseable owner/repo text: " <> owner_repo_text

    maybe_previously_posted_status <- liftIO $ SqlRead.get_posted_github_status db_connection_data owned_repo sha1

    let computation = do
          runExceptT $
            handleFailedStatuses
              db_connection_data
              access_token
              maybe_initiator
              owned_repo
              sha1
              maybe_previously_posted_status
          return ()

    -- Do not act on receipt of statuses from the context I have created, or else
    -- we may get stuck in an infinite notification loop
    --
    -- Also, if we haven't posted a summary status before, do not act unless the notification
    -- was for a failed build.
    let will_post = is_not_my_own_context && (is_failure_notification || not (null maybe_previously_posted_status))
    when will_post $ do
      _thread_id <- liftIO $ forkIO computation
      return ()

    return will_post

  where
    notified_status_state_string = LT.unpack $ Webhooks.state status_event
    is_failure_notification = notified_status_state_string == "failure"

    context_text = Webhooks.context status_event
    notified_status_context_string = LT.unpack context_text
    notified_status_context_text = LT.toStrict context_text
    is_not_my_own_context = notified_status_context_text /= myAppStatusContext
    sha1 = Builds.RawCommit $ LT.toStrict $ Webhooks.sha status_event


genFlakinessStatus ::
     Builds.RawCommit
  -> Int -- ^ flaky count
  -> Int -- ^ pre-broken count
  -> Int -- ^ total failure count
  -> StatusEvent.GitHubStatusEventSetter
genFlakinessStatus (Builds.RawCommit sha1) flaky_count pre_broken_count total_failcount =
  StatusEvent.GitHubStatusEventSetter
    description
    status_string
    (webserverBaseUrl <> "/commit-details.html?sha1=" <> LT.fromStrict sha1)
    (LT.fromStrict myAppStatusContext)

  where
    optional_kb_metric = if pre_broken_count > 0
      then [show pre_broken_count <> "/" <> show total_failcount <> " pre-broken"]
      else []

    metrics = intercalate ", " $ [
        show flaky_count <> "/" <> show total_failcount <> " flaky"
      ] ++ optional_kb_metric

    description = LT.pack $ unwords [
        "(experimental)"
      , metrics
      ]

    status_string = if flaky_count == total_failcount
      then "success"
      else "failure"


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
          will_post <- liftIO $ handleStatusWebhook connection_data (AuthConfig.personal_access_token github_config) Nothing body_json
          S.json =<< return ["Will post?" :: String, show will_post]

        "push" -> do

          body_json <- S.jsonData

          liftIO $ do
            putStrLn "Parsed push event body JSON..."
            handlePushWebhook connection_data (AuthConfig.personal_access_token github_config) body_json
            putStrLn "Handled push event."
          S.json =<< return ["hello" :: String]

        _ -> return ()
