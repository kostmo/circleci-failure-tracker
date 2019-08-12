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
import           Network.Wai.Session        (Session)
import qualified Web.Scotty                 as S
import qualified Web.Scotty.Internal.Types  as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbInsertion
import qualified GitRev
import qualified Pagination
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified StatusUpdate
import qualified WebApi
import qualified WeeklyStats


checkboxIsTrue :: Text -> Bool
checkboxIsTrue = (== ("true" :: Text))


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


echoEndpoint :: S.ScottyM ()
echoEndpoint = S.post "/api/echo" $ do
  body <- S.body
  headers <- S.headers

  liftIO $ do
    putStrLn "===== HEADERS ===="
    mapM_ (\(x, y) -> putStrLn $ LT.unpack $ x <> ": " <> y) headers
    putStrLn "== END HEADERS ==="
    putStrLn "====== BODY ======"
    putStrLn $ LBSC.unpack body
    putStrLn "==== END BODY ===="


getOffsetMode :: ScottyTypes.ActionT LT.Text IO Pagination.ParentOffsetMode
getOffsetMode = do
  offset_count <- S.param "offset"
  starting_commit <- S.param "sha1"
  use_sha1_offset <- S.param "use_sha1_offset"
  use_commit_index_bounds <- S.param "use_commit_index_bounds"
  min_commit_index <- S.param "min_commit_index"
  max_commit_index <- S.param "max_commit_index"
  commit_count <- S.param "count"

  let
    offset_mode
      | checkboxIsTrue use_sha1_offset = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Commit $ Builds.RawCommit starting_commit)
            commit_count
      | checkboxIsTrue use_commit_index_bounds = Pagination.CommitIndices $
          WeeklyStats.InclusiveNumericBounds
            min_commit_index
            max_commit_index
      | otherwise = Pagination.FixedAndOffset $
          Pagination.OffsetLimit
            (Pagination.Count offset_count)
            commit_count

  return offset_mode


jsonDbGet :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> ScottyTypes.RoutePattern
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO a)
  -> S.ScottyM ()
jsonDbGet connection_data endpoint_path f =

  S.get endpoint_path $ S.json =<< run_with_connection =<< f
  where
    wrapped_connection = liftIO $ DbHelpers.get_connection connection_data
    run_with_connection = liftIO . (=<< wrapped_connection) . runReaderT


jsonAuthorizedDbInteract :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO (ReaderT Connection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
jsonAuthorizedDbInteract connection_data session github_config f = do

  func <- f

  let callback_func _user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func conn

  rq <- S.request
  insertion_result <- liftIO $
    Auth.getAuthenticatedUser rq session github_config callback_func

  S.json $ WebApi.toJsonEither insertion_result


breakageCauseReport ::
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO ()
breakageCauseReport connection_data session github_config = do
    breakage_sha1 <- S.param "cause_sha1"
    notes <- S.param "notes"
    jobs_delimited <- S.param "jobs"

    is_ongoing_text <- S.param "is_ongoing"
    last_affected_sha1 <- S.param "last_affected_sha1"

    failure_mode_id <- S.param "failure_mode_id"

    let is_still_ongoing = checkboxIsTrue is_ongoing_text

    rq <- S.request
    insertion_result <- liftIO $ runExceptT $ do

      let callback_func user_alias = runExceptT $ do

            conn <- liftIO $ DbHelpers.get_connection connection_data
            SqlWrite.reportBreakage
              (SqlRead.AuthConnection conn user_alias)
              jobs_delimited
              failure_mode_id
              is_still_ongoing
              (Builds.RawCommit last_affected_sha1)
              (Builds.RawCommit breakage_sha1)
              notes

      ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
    S.json $ WebApi.toJsonEither insertion_result


breakageResolutionReport ::
     DbHelpers.DbConnectionData
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> ScottyTypes.ActionT LT.Text IO ()
breakageResolutionReport connection_data session github_config = do

    sha1 <- S.param "sha1"
    cause_ids_delimited <- S.param "causes"

    rq <- S.request
    insertion_result <- liftIO $ runExceptT $ do

      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            runReaderT
              (SqlWrite.apiCodeBreakageResolutionInsertMultiple sha1 cause_ids_delimited)
              (SqlRead.AuthConnection conn user_alias)


      ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
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
     DbHelpers.DbConnectionData
  -> AuthConfig.GithubConfig
  -> Vault.Key (Session IO String String)
  -> ScottyTypes.ActionT LT.Text IO (ReaderT SqlRead.AuthConnection IO (Either Text a))
  -> ScottyTypes.ActionT LT.Text IO ()
postWithAuthentication connection_data github_config session f = do

  func <- f
  let callback_func user_alias = do
        conn <- DbHelpers.get_connection connection_data
        runReaderT func $ SqlRead.AuthConnection conn user_alias

  rq <- S.request
  insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
  S.json $ DbInsertion.toInsertionResponse github_config insertion_result


rescanCommitCallback :: (MonadIO m) =>
     AuthConfig.GithubConfig
  -> Builds.RawCommit
  -> ReaderT SqlRead.AuthConnection m (Either Text Text)
rescanCommitCallback github_config commit = do
  SqlRead.AuthConnection conn user_alias <- ask

  liftIO $ do
    maybe_previously_posted_status <- SqlRead.getPostedGithubStatus
      conn
      owned_repo
      commit

    run_result <- runExceptT $
      StatusUpdate.handleFailedStatuses
        conn
        (AuthConfig.personal_access_token github_config)
        (Just user_alias)
        owned_repo
        commit
        maybe_previously_posted_status

    return $ first LT.toStrict $ run_result $> "Commit rescan complete."

  where
    owned_repo = DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name
