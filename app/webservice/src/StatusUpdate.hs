{-# LANGUAGE OverloadedStrings #-}

module StatusUpdate (
    github_event_endpoint
  , handleFailedStatuses
  ) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (guard, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Except    (ExceptT (ExceptT), except,
                                                runExceptT)
import qualified Data.ByteString.Lazy          as LBS
import           Data.List                     (filter, intercalate)
import           Data.List.Split               (splitOn)
import qualified Data.Maybe                    as Maybe
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Time.Clock               as Clock
import qualified GitHub.Data.Webhooks.Validate as GHValidate
import qualified Network.OAuth.OAuth2          as OAuth2
import qualified Network.URI                   as URI
import qualified Safe
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S
import           Web.Scotty.Internal.Types     (ActionT)

import qualified ApiPost
import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified DbHelpers
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified StatusEvent
import qualified StatusEventQuery
import qualified Webhooks


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


-- | XXX Note that we are obtaining the build metadata from an "unofficial"
-- source; the queued_at time is inaccurate and the branch name is empty.
get_circleci_failure :: Text -> StatusEventQuery.GitHubStatusEventGetter -> Maybe Builds.Build
get_circleci_failure sha1 event_setter = do
  guard $ circleCIContextPrefix `T.isPrefixOf` context_text
  parsed_uri <- URI.parseURI $ LT.unpack url_text

  uri_authority <- URI.uriAuthority parsed_uri
  let hostname = URI.uriRegName uri_authority
  guard $ hostname == circleciDomain

  last_segment <- Safe.lastMay $ splitOn "/" $ URI.uriPath parsed_uri
  build_number <- readMaybe last_segment
  return $ Builds.NewBuild (Builds.NewBuildNumber build_number) sha1 current_time build_name ""

  where
    context_text = LT.toStrict context
    build_name = T.drop (T.length circleCIContextPrefix) context_text

    current_time = StatusEventQuery._created_at event_setter
    context = StatusEventQuery._context event_setter
    url_text = StatusEventQuery._target_url event_setter


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
  -> Text
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

  build_statuses_list_any_source <- ExceptT $ Auth.getBuildStatuses access_token owned_repo sha1

  liftIO $ putStrLn $ "Build statuses count: " ++ show (length build_statuses_list_any_source)

  let statuses_list_not_mine = filter is_not_my_own_context build_statuses_list_any_source

      filter_failed = filter $ (== "failure") . StatusEventQuery._state
      failed_statuses_list_not_mine = filter_failed statuses_list_not_mine

      circleci_failed_builds = Maybe.mapMaybe (get_circleci_failure sha1) failed_statuses_list_not_mine
      scannable_build_numbers = map Builds.build_id circleci_failed_builds

      circleci_failcount = length circleci_failed_builds

  liftIO $ putStrLn $ "Failed CircleCI build count: " ++ show circleci_failcount


  builds_with_flaky_pattern_matches <- liftIO $ do
    conn <- DbHelpers.get_connection db_connection_data
    scan_resources <- Scanning.prepare_scan_resources conn maybe_initiator
    SqlWrite.store_builds_list conn circleci_failed_builds
    scan_matches <- Scanning.scan_builds scan_resources $ Left $ Set.fromList scannable_build_numbers

    -- TODO - we should instead see if the "best matching pattern" is
    -- flaky, rather than checking if *any* matching pattern is a
    -- "flaky" pattern.
    flaky_pattern_ids <- SqlRead.get_flaky_pattern_ids conn
    let flaky_predicate = any ((`Set.member` flaky_pattern_ids) . DbHelpers.db_id . ScanPatterns.scanned_pattern) . snd
        builds_with_flaky_pattern_matches = filter flaky_predicate scan_matches
    return builds_with_flaky_pattern_matches

  let flaky_count = length builds_with_flaky_pattern_matches
      status_setter_data = gen_flakiness_status (LT.fromStrict sha1) flaky_count circleci_failcount
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
    Just previous_state_description_tuple -> if previous_state_description_tuple /= new_state_description_tuple
      then post_and_store
      else return ()

  where
    is_not_my_own_context = (/= myAppStatusContext) . LT.toStrict . StatusEventQuery._context


handleStatusWebhook ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> Maybe AuthStages.Username
  -> Webhooks.GitHubStatusEvent
  -> IO (Either LT.Text ())
handleStatusWebhook db_connection_data access_token maybe_initiator status_event = do

  runExceptT $ do

    liftIO $ putStrLn $ "Notified status context was: " ++ notified_status_context_string

    let notified_status_url_string = LT.unpack $ Webhooks.target_url status_event
    when (circleCIContextPrefix `T.isPrefixOf` notified_status_context_text) $ do

      liftIO $ putStrLn $ "CircleCI URL was: " ++ notified_status_url_string


    let owner_repo_text = Webhooks.name status_event
        splitted = splitOn "/" $ LT.unpack owner_repo_text

    owned_repo <- except $ case splitted of
      (org:repo:[]) -> Right $ DbHelpers.OwnerAndRepo org repo
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
    when (is_not_my_own_context && (is_failure_notification || not (null maybe_previously_posted_status))) $ do
      _thread_id <- liftIO $ forkIO computation
      return ()

    return ()

  where
    notified_status_state_string = LT.unpack $ Webhooks.state status_event
    is_failure_notification = notified_status_state_string == "failure"

    context_text = Webhooks.context status_event
    notified_status_context_string = LT.unpack context_text
    notified_status_context_text = LT.toStrict context_text
    is_not_my_own_context = notified_status_context_text /= myAppStatusContext
    sha1 = LT.toStrict $ Webhooks.sha status_event


gen_flakiness_status ::
     LT.Text
  -> Int
  -> Int
  -> StatusEvent.GitHubStatusEventSetter
gen_flakiness_status sha1 flaky_count total_failcount =
  StatusEvent.GitHubStatusEventSetter
    description
    status_string
    (webserverBaseUrl <> "/commit-details.html?sha1=" <> sha1)
    (LT.fromStrict myAppStatusContext)

  where
    metrics = intercalate ", " [
        show flaky_count <> "/" <> show total_failcount <> " flaky"
--      , show 0 <> "/" <> show 0 <> " KPs"
      ]
    description = LT.pack $ intercalate " " [
        "(experimental)"
      , metrics
      ]
    status_string = if flaky_count == total_failcount
      then "success"
      else "failure"


github_event_endpoint :: DbHelpers.DbConnectionData -> AuthConfig.GithubConfig -> ActionT LT.Text IO ()
github_event_endpoint connection_data github_config = do

    maybe_signature_header <- S.header "X-Hub-Signature"
    rq_body <- S.body

    let is_signature_valid = GHValidate.isValidPayload
          (AuthConfig.webhook_secret github_config)
          (LT.toStrict <$> maybe_signature_header)
          (LBS.toStrict rq_body)

    maybe_event_type <- S.header "X-GitHub-Event"
    case maybe_event_type of
      Nothing -> return ()
      Just event_type -> if not is_signature_valid
        then return ()
        else if event_type /= "status"
          then do
            S.json =<< return ["hello" :: String] -- XXX Do I even need to send a response?
          else do
            body_json <- S.jsonData
            liftIO $ handleStatusWebhook connection_data (AuthConfig.personal_access_token github_config) Nothing body_json
            S.json =<< return ["hello" :: String]
