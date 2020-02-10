{-# LANGUAGE OverloadedStrings #-}

module FrontendHelpers where

import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Either.Utils          (maybeToEither)
import           Data.Functor               (($>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Vault.Lazy            as Vault
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wai.Session        (Session)
import qualified Web.Scotty                 as S
import qualified Web.Scotty.Internal.Types  as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified CircleApi
import qualified CircleTrigger
import qualified Constants
import qualified DbHelpers
import qualified DbInsertion
import qualified DebugUtils                 as D
import qualified GitRev
import qualified Pagination
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified StatusUpdate
import qualified WebApi


checkboxIsTrue :: Text -> Bool
checkboxIsTrue = (== ("true" :: Text))


apiGetSqlTimeoutSeconds :: Integer
apiGetSqlTimeoutSeconds = 3 * 60


data MutablePatternParms = MutablePatternParms {
    pat_is_nondeterminisitc :: Bool
  , pat_description         :: Text
  , pat_tags_raw_text       :: String
  , pat_specificity         :: Int
  }


getMutablePatternParms :: ScottyTypes.ActionT LT.Text IO MutablePatternParms
getMutablePatternParms = do
  is_nondeterministic <- checkboxIsTrue <$> S.param "is_nondeterministic"

  MutablePatternParms is_nondeterministic
    <$> S.param "description"
    <*> S.param "tags"
    <*> S.param "specificity"


patternFromParms :: ScottyTypes.ActionT LT.Text IO ScanPatterns.Pattern
patternFromParms = do

  mutable_pattern_parms <- getMutablePatternParms

  expression <- S.param "pattern"

  liftIO $ D.debugList [
      "Pattern expression is:"
    , show expression
    ]

  is_regex <- checkboxIsTrue <$> S.param "is_regex"

  use_lines_from_end <- checkboxIsTrue <$> S.param "use_lines_from_end"
  applicable_steps <- S.param "applicable_steps"

  let match_expression = ScanPatterns.toMatchExpression is_regex expression $ pat_is_nondeterminisitc mutable_pattern_parms

  lines_from_end <- if use_lines_from_end
    then Just <$> S.param "lines_from_end"
    else return Nothing

  return $ ScanPatterns.NewPattern
    match_expression
    (pat_description mutable_pattern_parms)
    (DbHelpers.cleanSemicolonDelimitedList $ pat_tags_raw_text mutable_pattern_parms)
    (DbHelpers.cleanSemicolonDelimitedList applicable_steps)
    (pat_specificity mutable_pattern_parms)
    False
    lines_from_end


validateMaybeRevision ::
     LT.Text
  -> ScottyTypes.ActionT LT.Text IO (Either Text (Maybe GitRev.GitSha1))
validateMaybeRevision key = do
  implicated_revision <- S.param key
  return $ if T.null implicated_revision
    then Right Nothing
    else do
      validated_revision <- GitRev.validateSha1 implicated_revision
      return $ Just validated_revision


printBodyAndHeaders :: ScottyTypes.ActionT LT.Text IO ()
printBodyAndHeaders = do
  body <- S.body
  query_parms <- S.params
  headers <- S.headers

  liftIO $ do
    putStrLn "===== HEADERS ===="
    mapM_ (\(x, y) -> putStrLn $ LT.unpack $ x <> ": " <> y) headers
    putStrLn "== END HEADERS ==="

    putStrLn "===== QUERY PARAMETERS ===="
    mapM_ (\(x, y) -> putStrLn $ LT.unpack $ x <> ": " <> y) query_parms
    putStrLn "== END QUERY PARAMETERS ==="

    putStrLn "====== BODY ======"
    putStrLn $ LBSC.unpack body
    putStrLn "==== END BODY ===="


echoEndpoint :: S.ScottyM ()
echoEndpoint = S.post "/api/echo" printBodyAndHeaders


getSimpleOffsetMode :: ScottyTypes.ActionT LT.Text IO Pagination.ParentOffsetMode
getSimpleOffsetMode = do
  offset_count <- S.param "offset"
  commit_count <- S.param "count"
  return $ Pagination.FixedAndOffset $ Pagination.OffsetLimit
    (Pagination.Count offset_count)
    commit_count


getOffsetMode :: ScottyTypes.ActionT LT.Text IO Pagination.TimelineParms
getOffsetMode = do
  offset_count <- S.param "offset"
  starting_commit <- S.param "sha1"
  use_sha1_offset <- S.param "use_sha1_offset"
  use_commit_index_bounds <- S.param "use_commit_index_bounds"
  min_commit_index <- S.param "min_commit_index"
  max_commit_index <- S.param "max_commit_index"
  commit_count <- S.param "count"
  should_suppress_scheduled_builds_text <- S.param "should_suppress_scheduled_builds"

  should_use_uncached_annotations_text <- S.param "should_use_uncached_annotations"

  should_suppress_fully_successful_columns_text <- S.param "should_suppress_fully_successful_columns"
  max_columns_suppress_successful <- S.param "max_columns_suppress_successful"

  let
    offset_mode
      | checkboxIsTrue use_sha1_offset = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Commit $ Builds.RawCommit starting_commit)
            commit_count
      | checkboxIsTrue use_commit_index_bounds = Pagination.CommitIndices $
          DbHelpers.InclusiveNumericBounds
            min_commit_index
            max_commit_index
      | otherwise = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Count offset_count)
            commit_count

    maybe_successful_column_suppression = if checkboxIsTrue should_suppress_fully_successful_columns_text
      then Just max_columns_suppress_successful
      else Nothing
    column_filtering_options = Pagination.ColumnFilteringOptions
      (checkboxIsTrue should_suppress_scheduled_builds_text)
      maybe_successful_column_suppression

  return $ Pagination.TimelineParms
    column_filtering_options
    (checkboxIsTrue should_use_uncached_annotations_text)
    offset_mode


facilitateJobRebuild ::
     CircleApi.CircleCIApiToken
  -> Builds.UniversalBuildId
  -> SqlRead.AuthDbIO (Either T.Text Int64)
facilitateJobRebuild circleci_api_token universal_build_id = do
  dbauth@(SqlRead.AuthConnection conn _user) <- ask
  liftIO $ fmap (first T.pack) $ runExceptT $ do
    storable_build <- ExceptT $ fmap (first T.unpack) $
      flip runReaderT conn $ SqlRead.getGlobalBuild universal_build_id

    let provider_build_num = Builds.build_id $ Builds.build_record storable_build

    circleci_responses <- CircleTrigger.rebuildCircleJobsInWorkflow
      circleci_api_token
      [provider_build_num]

    result <- ExceptT $ flip runReaderT dbauth $
      SqlWrite.insertRebuildTriggerEvent
        universal_build_id
        (T.intercalate "; " $ map (CircleTrigger.message . snd)  circleci_responses)

    liftIO $ D.debugStr "Submitted rebuild request."
    return result


getLoggedInUser :: SqlRead.AuthDbIO (Either T.Text AuthStages.Username)
getLoggedInUser = do
  SqlRead.AuthConnection _conn user <- ask
  return $ Right user


jsonDbGet :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> ScottyTypes.RoutePattern
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO a)
  -> S.ScottyM ()
jsonDbGet connection_data endpoint_path f =

  S.get endpoint_path $ S.json =<< run_with_connection =<< f
  where
    wrapped_connection = liftIO $ DbHelpers.getConnectionWithStatementTimeout connection_data apiGetSqlTimeoutSeconds
    run_with_connection = liftIO . (=<< wrapped_connection) . runReaderT


-- | Compare to: "postWithAuthentication"
jsonAuthorizedDbInteract2 :: ToJSON a =>
     AuthHelperBundle
  -> ScottyTypes.ActionT LT.Text IO (ReaderT SqlRead.AuthConnection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
jsonAuthorizedDbInteract2
    (AuthHelperBundle connection_data session github_config third_party_creds)
    f = do

  func <- f

  let callback_func user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func $ SqlRead.AuthConnection conn user_alias

  login_redirect_path <- S.param "login_redirect_path"

  rq <- S.request

  insertion_result <- liftIO $
    Auth.getAuthenticatedUser
      login_redirect_path
      rq
      session
      github_config
      third_party_creds
      callback_func

  S.json $ WebApi.toJsonEither insertion_result


jsonAuthorizedDbInteract :: ToJSON a =>
     AuthHelperBundle
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
jsonAuthorizedDbInteract
    (AuthHelperBundle connection_data session github_config third_party_creds)
    f = do

  func <- f

  let callback_func _user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func conn

  login_redirect_path <- S.param "login_redirect_path"

  rq <- S.request

  insertion_result <- liftIO $
    Auth.getAuthenticatedUser
      login_redirect_path
      rq
      session
      github_config
      third_party_creds
      callback_func

  S.json $ WebApi.toJsonEither insertion_result


data AuthHelperBundle = AuthHelperBundle {
    conn_data               :: DbHelpers.DbConnectionData
  , session_vault           :: Vault.Key (Session IO String String)
  , github_conf             :: AuthConfig.GithubConfig
  , third_party_credentials :: CircleApi.ThirdPartyAuth
  }


breakageCauseReport ::
     AuthHelperBundle
  -> DbHelpers.DbConnectionData
  -> ScottyTypes.ActionT LT.Text IO ()
breakageCauseReport
    (AuthHelperBundle connection_data session github_config third_party_creds)
    mview_connection_data = do

  breakage_sha1 <- S.param "cause_sha1"
  notes <- S.param "notes"
  jobs_delimited <- S.param "jobs"

  is_ongoing_text <- S.param "is_ongoing"
  last_affected_sha1 <- S.param "last_affected_sha1"

  failure_mode_id <- S.param "failure_mode_id"

  login_redirect_path <- S.param "login_redirect_path"

  let is_still_ongoing = checkboxIsTrue is_ongoing_text

  rq <- S.request
  insertion_result <- liftIO $ runExceptT $ do

    let callback_func user_alias = runExceptT $ do

          conn <- liftIO $ DbHelpers.get_connection connection_data
          result <- SqlWrite.reportBreakage
            (SqlRead.AuthConnection conn user_alias)
            jobs_delimited
            failure_mode_id
            is_still_ongoing
            (Builds.RawCommit last_affected_sha1)
            (Builds.RawCommit breakage_sha1)
            notes

          -- FIXME This is too annoying to wait for;
          -- should be kicked off asynchronously
          {-
          liftIO $ do
            mview_conn <- DbHelpers.get_connection mview_connection_data
            SqlRead.refreshCachedMasterGrid mview_conn
          -}
          return result

    ExceptT $ Auth.getAuthenticatedUser
      login_redirect_path
      rq
      session
      github_config
      third_party_creds
      callback_func

  S.json $ WebApi.toJsonEither insertion_result


breakageResolutionReport ::
     AuthHelperBundle
  -> DbHelpers.DbConnectionData
  -> ScottyTypes.ActionT LT.Text IO ()
breakageResolutionReport
    (AuthHelperBundle connection_data session github_config third_party_creds)
    mview_connection_data = do

  sha1 <- S.param "sha1"
  cause_ids_delimited <- S.param "causes"

  login_redirect_path <- S.param "login_redirect_path"

  rq <- S.request
  insertion_result <- liftIO $ runExceptT $ do

    let callback_func user_alias = do
          conn <- DbHelpers.get_connection connection_data
          result <- runReaderT
            (SqlWrite.apiCodeBreakageResolutionInsertMultiple sha1 cause_ids_delimited)
            (SqlRead.AuthConnection conn user_alias)

          -- FIXME This is too annoying to wait for;
          -- should be kicked off asynchronously
          {-
          mview_conn <- DbHelpers.get_connection mview_connection_data
          SqlRead.refreshCachedMasterGrid mview_conn
          -}
          return result

    ExceptT $ Auth.getAuthenticatedUser
      login_redirect_path
      rq
      session
      github_config
      third_party_creds
      callback_func

  S.json $ WebApi.toJsonEither insertion_result


requireAdminToken :: (FromJSON t, ToJSON b) =>
     DbHelpers.DbConnectionData
  -> AuthConfig.GithubConfig
  -> (Connection -> t -> IO (Either Text b))
  -> ScottyTypes.ActionT LT.Text IO ()
requireAdminToken connection_data github_config f = do
    body_json <- S.jsonData
    maybe_auth_header <- S.header "token"

    insertion_result <- liftIO $ runExceptT $ do
        auth_token <- except $ maybeToEither (T.pack "Need \"token\" header!") maybe_auth_header
        when (LT.toStrict auth_token /= AuthConfig.admin_password github_config) $
          except $ Left $ T.pack "Incorrect admin password"

        ExceptT $ do
          conn <- liftIO $ DbHelpers.get_connection connection_data
          f conn body_json

    S.json $ WebApi.toJsonEither insertion_result


postWithAuthentication :: ToJSON a =>
     AuthHelperBundle
  -> ScottyTypes.ActionT LT.Text IO (ReaderT SqlRead.AuthConnection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
postWithAuthentication
    (AuthHelperBundle connection_data session github_config third_party_creds)
    f = do

  func <- f
  let callback_func user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func $ SqlRead.AuthConnection conn user_alias

  login_redirect_path <- S.param "login_redirect_path"

  rq <- S.request

  insertion_result <- liftIO $ Auth.getAuthenticatedUser
    login_redirect_path
    rq
    session
    github_config
    third_party_creds
    callback_func

  S.json $ DbInsertion.toInsertionResponse
    login_redirect_path
    github_config
    insertion_result


rescanCommitCallback :: (MonadIO m) =>
     CircleApi.ThirdPartyAuth
  -> Builds.RawCommit
  -> ReaderT SqlRead.AuthConnection m (Either Text Text)
rescanCommitCallback third_party_auth commit = do
  SqlRead.AuthConnection conn user_alias <- ask

  liftIO $ do

    D.debugList ["Starting rescanCommitCallback"]

    run_result <- runExceptT $
      StatusUpdate.readGitHubStatusesAndScanAndPostSummaryForCommit
        third_party_auth
        conn
        (Just user_alias)
        owned_repo
        StatusUpdate.ShouldStoreDetailedSuccessRecords
        commit
        StatusUpdate.ShouldScanLogs
        Scanning.RevisitScanned

    return $ first LT.toStrict $ run_result $> "Commit rescan complete."

  where
    owned_repo = DbHelpers.OwnerAndRepo Constants.projectName Constants.repoName
