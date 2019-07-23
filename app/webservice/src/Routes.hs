{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT (ExceptT), except,
                                                  runExceptT)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Control.Monad.Trans.Reader      (ReaderT)
import           Data.Aeson                      (ToJSON)
import           Data.Bifunctor                  (first)
import qualified Data.ByteString.Lazy.Char8      as LBSC
import           Data.Default                    (def)
import           Data.Either.Utils               (maybeToEither)
import           Data.Functor                    (($>))
import           Data.List                       (filter)
import           Data.List.Split                 (splitOn)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Vault.Lazy                 as Vault
import           Database.PostgreSQL.Simple      (Connection)
import           Network.Wai
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.Static   hiding ((<|>))
import           Network.Wai.Session             (Session, SessionStore,
                                                  withSession)
import           System.FilePath
import qualified Web.Scotty                      as S
import qualified Web.Scotty.Internal.Types       as ScottyTypes

import qualified Auth
import qualified AuthConfig
import qualified AuthStages
import qualified Breakages
import qualified Breakages2
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbInsertion
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Pagination
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusUpdate
import qualified Types
import qualified WebApi
import qualified WeeklyStats


data SetupData = SetupData {
    _setup_static_base     :: String
  , _setup_github_config   :: AuthConfig.GithubConfig
  , _setup_connection_data :: DbHelpers.DbConnectionData
  }


data PersistenceData = PersistenceData {
    _setup_cache   :: Types.CacheStore
  , _setup_session :: Vault.Key (Session IO String String)
  , _setup_store   :: SessionStore IO String String
  }


data MutablePatternParms = MutablePatternParms {
    pat_is_nondeterminisitc :: Bool
  , pat_description         :: Text
  , pat_tags_raw_text       :: String
  , pat_specificity         :: Int
  }


checkboxIsTrue :: Text -> Bool
checkboxIsTrue = (== ("true" :: Text))


getMutablePatternParms :: ScottyTypes.ActionT LT.Text IO MutablePatternParms
getMutablePatternParms = do
  is_nondeterministic <- checkboxIsTrue <$> S.param "is_nondeterministic"
  description <- S.param "description"
  specificity <- S.param "specificity"
  tags <- S.param "tags"
  return $ MutablePatternParms
    is_nondeterministic
    description
    tags
    specificity


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
    (clean_list $ pat_tags_raw_text mutable_pattern_parms)
    (clean_list applicable_steps)
    (pat_specificity mutable_pattern_parms)
    False
    lines_from_end
  where
    clean_list = filter (not . T.null) . map (T.strip . T.pack) . splitOn ";"


validateMaybeRevision :: LT.Text -> ScottyTypes.ActionT LT.Text IO (Either Text (Maybe GitRev.GitSha1))
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


retrieveLogContext ::
     Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> DbHelpers.DbConnectionData
  -> ScottyTypes.ActionT LT.Text IO ()
retrieveLogContext session github_config connection_data = do
  match_id <- S.param "match_id"
  context_linecount <- S.param "context_linecount"

  let callback_func :: AuthStages.Username -> IO (Either Text SqlRead.LogContext)
      callback_func _user_alias = SqlRead.log_context_func
        connection_data
        (MatchOccurrences.MatchId match_id)
        context_linecount

  rq <- S.request
  either_log_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
  S.json $ WebApi.toJsonEither either_log_result


jsonDbGet :: ToJSON a =>
     DbHelpers.DbConnectionData
  -> ScottyTypes.RoutePattern
  -> ReaderT Connection IO a
  -> S.ScottyM ()
jsonDbGet connection_data endpoint_path f =
  S.get endpoint_path $ do
    conn <- liftIO $ DbHelpers.get_connection connection_data
    x <- liftIO $ runReaderT f conn
    S.json x


jsonDbGet1 connection_data endpoint_path parm_name f =
  S.get endpoint_path $ do
    parm1 <- S.param parm_name

    conn <- liftIO $ DbHelpers.get_connection connection_data
    x <- liftIO $ runReaderT (f parm1) conn
    S.json x


jsonDbGet1Either connection_data endpoint_path parm_name f =
  S.get endpoint_path $ do
    parm1 <- S.param parm_name

    conn <- liftIO $ DbHelpers.get_connection connection_data
    x <- liftIO $ runReaderT (f parm1) conn
    S.json $ WebApi.toJsonEither x


scottyApp :: PersistenceData -> SetupData -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp (PersistenceData cache session store) (SetupData static_base github_config connection_data) = do

    S.middleware $ withSession store (fromString "SESSION") def session

    S.middleware $ staticPolicy $ noDots >-> addBase static_base

    unless (AuthConfig.is_local github_config) $
      S.middleware forceSSL


    -- For debugging only
    when (AuthConfig.is_local github_config) echoEndpoint


    S.post "/api/github-event" $ StatusUpdate.github_event_endpoint connection_data github_config



    -- Experimental
    S.post "/api/report-breakage" $ do

      either_maybe_implicated_revision <- validateMaybeRevision "implicated_revision"
      notes <- S.param "notes"
      is_broken <- S.param "is_broken"
      step_id <- S.param "step_id"

      rq <- S.request
      insertion_result <- liftIO $ runExceptT $ do
        maybe_implicated_revision <- except $ first AuthStages.DbFailure either_maybe_implicated_revision

        let callback_func user_alias = SqlWrite.api_new_breakage_report connection_data breakage_report
              where
                breakage_report = Breakages.NewBreakageReport
                  (Builds.NewBuildStepId step_id)
                  (GitRev.sha1 <$> maybe_implicated_revision)
                  is_broken
                  notes
                  user_alias

        ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result



    S.post "/api/code-breakage-resolution-report" $ do

      sha1 <- S.param "sha1"
      cause_ids_delimited <- S.param "causes"

      let clean_list = filter (not . T.null) . map (T.strip . T.pack) . splitOn ";"
          cause_id_list = map (read . T.unpack) $ clean_list cause_ids_delimited

      rq <- S.request
      insertion_result <- liftIO $ runExceptT $ do

        let callback_func user_alias = do
              insertion_eithers <- mapM (SqlWrite.api_code_breakage_resolution_insert connection_data . gen_resolution_report) cause_id_list
              return $ sequenceA insertion_eithers
              where
                gen_resolution_report cause_id = Breakages2.NewResolutionReport
                  sha1
                  cause_id
                  user_alias

        ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/code-breakage-cause-report" $ do

      sha1 <- S.param "sha1"
      description <- S.param "description"
      jobs_delimited <- S.param "jobs"

      let clean_list = filter (not . T.null) . map (T.strip . T.pack) . splitOn ";"
          jobs_list = clean_list jobs_delimited


      rq <- S.request
      insertion_result <- liftIO $ runExceptT $ do

        let callback_func user_alias = SqlWrite.api_code_breakage_cause_insert connection_data breakage_report jobs_list
              where
                breakage_report = Breakages2.NewBreakageReport
                  sha1
                  description
                  user_alias

        ExceptT $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result



    S.post "/api/rescan-build" $ do

      build_number <- S.param "build"

      let callback_func :: AuthStages.Username -> IO (Either (AuthStages.BackendFailure Text) Text)
          callback_func user_alias = do
            Scanning.rescanSingleBuild
              connection_data
              user_alias
              (Builds.NewBuildNumber build_number)
            return $ Right "Scan complete."

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/rescan-commit" $ do

      commit_sha1_text <- S.param "sha1"

      let owned_repo = DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name

          callback_func :: AuthStages.Username -> IO (Either (AuthStages.BackendFailure Text) Text)
          callback_func user_alias = do
            maybe_previously_posted_status <- liftIO $ SqlRead.get_posted_github_status connection_data owned_repo commit_sha1_text

            run_result <- runExceptT $
              StatusUpdate.handleFailedStatuses
                connection_data
                (AuthConfig.personal_access_token github_config)
                (Just user_alias)
                owned_repo
                commit_sha1_text
                maybe_previously_posted_status

            putStrLn $ "Run result: " ++ show run_result
            return $ first (AuthStages.DbFailure . LT.toStrict) $ run_result $> "Finished scan"

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/populate-master-commits" $ do
      body_json <- S.jsonData
      maybe_auth_header <- S.header "token"

      insertion_result <- liftIO $ runExceptT $ do
          auth_token <- except $ maybeToEither (T.pack "Need \"token\" header!") maybe_auth_header
          when (LT.toStrict auth_token /= AuthConfig.admin_password github_config) $
            except $ Left $ T.pack "Incorrect admin password"

          ExceptT $ do
            conn <- liftIO $ DbHelpers.get_connection connection_data
            SqlWrite.storeMasterCommits conn body_json

      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/populate-master-commit-metadata" $ do
      body_json <- S.jsonData
      maybe_auth_header <- S.header "token"

      insertion_result <- liftIO $ runExceptT $ do
          auth_token <- except $ maybeToEither (T.pack "Need \"token\" header!") maybe_auth_header
          when (LT.toStrict auth_token /= AuthConfig.admin_password github_config) $
            except $ Left $ T.pack "Incorrect admin password"
          ExceptT $ SqlWrite.storeCommitMetadata connection_data body_json

      S.json $ WebApi.toJsonEither insertion_result


    -- TODO FINISH ME
    {-
    S.post "/api/new-pattern-replace" $ do

      new_pattern <- patternFromParms
      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            SqlWrite.copy_pattern xxx

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ DbInsertion.toInsertionResponse github_config insertion_result
    -}



    S.post "/api/new-pattern-insert" $ do

      new_pattern <- patternFromParms
      let callback_func user_alias = do
            conn <- DbHelpers.get_connection connection_data
            SqlWrite.api_new_pattern conn $ Left (new_pattern, user_alias)

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ DbInsertion.toInsertionResponse github_config insertion_result


    -- XXX IMPORTANT:
    -- The session cookie is specific to the parent dir of the path.
    -- So with the path "/api/callback", only HTTP accesses to paths
    -- at or below the "/api/" path will be members of the same session.
    -- Consequentially, a cookie set (namely, the github access token)
    -- in a request to a certain path will only be accessible to
    -- other requests at or below that same parent directory.
    S.get "/api/github-auth-callback" $ do
      rq <- S.request
      let Just (_sessionLookup, sessionInsert) = Vault.lookup session $ vault rq
      Auth.callbackH cache github_config $ sessionInsert Auth.githubAuthTokenSessionKey

    S.get "/logout" $ Auth.logoutH cache

    jsonDbGet connection_data "/api/latest-master-commit-with-metadata" (WebApi.toJsonEither <$> SqlRead.get_latest_master_commit_with_metadata)

    jsonDbGet connection_data "/api/status-posted-commits-by-day" SqlRead.api_status_posted_commits_by_day

    jsonDbGet connection_data "/api/status-postings-by-day" SqlRead.api_status_postings_by_day

    jsonDbGet connection_data "/api/failed-commits-by-day" SqlRead.api_failed_commits_by_day

    jsonDbGet1Either connection_data "/api/test-failures" "pattern_id" $ SqlRead.apiTestFailures . ScanPatterns.PatternId

    jsonDbGet1 connection_data "/api/posted-statuses" "count" SqlRead.apiPostedStatuses

    jsonDbGet1 connection_data "/api/aggregate-posted-statuses" "count" SqlRead.apiAggregatePostedStatuses

    jsonDbGet1 connection_data "/api/list-commit-jobs" "sha1" $ SqlRead.apiCommitJobs . Builds.RawCommit

    jsonDbGet connection_data "/api/job" SqlRead.apiJobs

    jsonDbGet connection_data "/api/log-storage-stats" SqlRead.api_storage_stats

    jsonDbGet connection_data "/api/log-size-histogram" SqlRead.apiByteCountHistogram

    jsonDbGet connection_data "/api/log-lines-histogram" SqlRead.apiLineCountHistogram

    jsonDbGet1 connection_data "/api/pattern-step-occurrences" "pattern_id" $ SqlRead.patternBuildStepOccurrences . ScanPatterns.PatternId

    jsonDbGet1 connection_data "/api/pattern-job-occurrences" "pattern_id" $ SqlRead.patternBuildJobOccurrences . ScanPatterns.PatternId

    S.get "/api/master-timeline" $ do

      offset_count <- S.param "offset"
      starting_commit <- S.param "sha1"
      use_sha1_offset <- S.param "use_sha1_offset"
      use_commit_index_bounds <- S.param "use_commit_index_bounds"
      min_commit_index <- S.param "min_commit_index"
      max_commit_index <- S.param "max_commit_index"
      commit_count <- S.param "count"

      let offset_mode
            | checkboxIsTrue use_sha1_offset = Pagination.FixedAndOffset $ Pagination.OffsetLimit (Pagination.Commit $ Builds.RawCommit starting_commit) commit_count
            | checkboxIsTrue use_commit_index_bounds = Pagination.CommitIndices $ WeeklyStats.InclusiveNumericBounds min_commit_index max_commit_index
            | otherwise = Pagination.FixedAndOffset $ Pagination.OffsetLimit (Pagination.Count offset_count) commit_count


      json_result <- liftIO $ do
        conn <- DbHelpers.get_connection connection_data
        runReaderT (SqlRead.apiMasterBuilds offset_mode) conn

      S.json $ WebApi.toJsonEither json_result

    jsonDbGet connection_data "/api/step" SqlRead.apiStep

    S.get "/api/commit-info" $ do
      commit_sha1_text <- S.param "sha1"
      json_result <- liftIO $ runExceptT $ do
        sha1 <- except $ GitRev.validateSha1 commit_sha1_text
        ExceptT $ SqlUpdate.count_revision_builds connection_data (AuthConfig.personal_access_token github_config) sha1

      S.json $ WebApi.toJsonEither json_result

    S.get "/api/commit-builds" $ do
      commit_sha1_text <- S.param "sha1"
      json_result <- runExceptT $ do
        sha1 <- except $ GitRev.validateSha1 commit_sha1_text
        liftIO $ SqlRead.get_revision_builds connection_data sha1

      S.json $ WebApi.toJsonEither json_result

    S.get "/api/new-pattern-test" $ do
      liftIO $ putStrLn "Testing pattern..."
      buildnum <- S.param "build_num"
      new_pattern <- patternFromParms
      S.json =<< liftIO (do
        foo <- SqlRead.api_new_pattern_test connection_data (Builds.NewBuildNumber buildnum) new_pattern
        return $ WebApi.toJsonEither foo)

    jsonDbGet connection_data "/api/tags" SqlRead.apiTagsHistogram

    jsonDbGet1 connection_data "/api/tag-suggest" "term" SqlRead.api_autocomplete_tags

    jsonDbGet1 connection_data "/api/step-suggest" "term" SqlRead.api_autocomplete_steps

    jsonDbGet connection_data "/api/step-list" SqlRead.api_list_steps

    jsonDbGet1 connection_data "/api/branch-suggest" "term" SqlRead.api_autocomplete_branches

    jsonDbGet connection_data "/api/random-scannable-build" SqlRead.api_random_scannable_build

    jsonDbGet connection_data "/api/summary" SqlRead.api_summary_stats

    jsonDbGet connection_data "/api/master-build-stats" SqlRead.masterBuildFailureStats

    jsonDbGet1 connection_data "/api/master-weekly-failure-stats" "weeks" SqlRead.masterWeeklyFailureStats

    jsonDbGet connection_data "/api/unmatched-builds" SqlRead.api_unmatched_builds

    jsonDbGet1 connection_data "/api/unmatched-builds-for-commit" "sha1" SqlRead.api_unmatched_commit_builds

    jsonDbGet connection_data "/api/idiopathic-failed-builds" SqlRead.api_idiopathic_builds

    jsonDbGet1 connection_data "/api/idiopathic-failed-builds-for-commit" "sha1" SqlRead.api_idiopathic_commit_builds

    jsonDbGet1 connection_data "/api/timed-out-builds-for-commit" "sha1" SqlRead.api_timeout_commit_builds

    -- Access-controlled endpoint
    S.get "/api/view-log-context" $ retrieveLogContext session github_config connection_data


    S.get "/api/view-log-full" $ do
      build_id <- S.param "build_id"

      let callback_func :: AuthStages.Username -> IO (Either Text Text)
          callback_func _user_alias = do
            conn <- DbHelpers.get_connection connection_data
            maybe_log <- SqlRead.read_log conn $ Builds.NewBuildNumber build_id
            return $ maybeToEither "log not in database" maybe_log

      rq <- S.request
      either_log_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      case either_log_result of
        Right logs  -> S.text $ LT.fromStrict logs
        Left errors -> S.html $ LT.fromStrict $ JsonUtils._message $ JsonUtils.getDetails errors


    jsonDbGet1 connection_data "/api/pattern" "pattern_id" $ SqlRead.api_single_pattern . ScanPatterns.PatternId

    jsonDbGet connection_data "/api/patterns-dump" SqlRead.dumpPatterns

    -- XXX This is the deprecated kind of breakage report
    S.get "/api/breakages-dump" $
      S.json =<< liftIO (SqlRead.dump_breakages connection_data)

    jsonDbGet connection_data "/api/presumed-stable-branches-dump" SqlRead.dump_presumed_stable_branches

    S.post "/api/pattern-specificity-update" $ do
      pattern_id <- S.param "pattern_id"
      specificity <- S.param "specificity"
      let callback_func _user_alias = SqlWrite.update_pattern_specificity connection_data (ScanPatterns.PatternId pattern_id) specificity

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.post "/api/pattern-description-update" $ do
      pattern_id <- S.param "pattern_id"
      description <- S.param "description"
      let callback_func _user_alias = SqlWrite.update_pattern_description connection_data (ScanPatterns.PatternId pattern_id) description

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.post "/api/pattern-tag-add" $ do
      pattern_id <- S.param "pattern_id"
      tag <- S.param "tag"
      let callback_func _user_alias = SqlWrite.add_pattern_tag connection_data (ScanPatterns.PatternId pattern_id) tag

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.post "/api/pattern-tag-remove" $ do
      pattern_id <- S.param "pattern_id"
      tag <- S.param "tag"
      let callback_func _user_alias = SqlWrite.remove_pattern_tag connection_data (ScanPatterns.PatternId pattern_id) tag

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    S.post "/api/patterns-restore" $ do
      body_json <- S.jsonData

      let callback_func _user_alias = SqlWrite.restore_patterns connection_data body_json

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result

    jsonDbGet connection_data "/api/code-breakages-detected" SqlRead.apiDetectedCodeBreakages

    jsonDbGet connection_data "/api/code-breakages-annotated" SqlRead.apiAnnotatedCodeBreakages

    jsonDbGet1 connection_data "/api/known-breakage-affected-jobs" "cause_id" SqlRead.knownBreakageAffectedJobs

    S.post "/api/code-breakage-job-delete" $ do
      cause_id <- S.param "cause_id"
      job <- S.param "job"

      let callback_func _user_alias = SqlWrite.deleteCodeBreakageJob connection_data cause_id job

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/code-breakage-description-update" $ do
      item_id <- S.param "cause_id"
      description <- S.param "description"
      let callback_func _user_alias = SqlWrite.update_code_breakage_description connection_data item_id description

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    S.post "/api/code-breakage-delete" $ do
      item_id <- S.param "cause_id"
      let callback_func _user_alias = SqlWrite.deleteCodeBreakage connection_data item_id

      rq <- S.request
      insertion_result <- liftIO $ Auth.getAuthenticatedUser rq session github_config callback_func
      S.json $ WebApi.toJsonEither insertion_result


    -- XXX This is the deprecated kind of breakage report
    S.get "/api/commit-breakage-reports" $ do
      commit_sha1_text <- S.param "sha1"
      S.json =<< liftIO (SqlRead.api_commit_breakage_reports connection_data commit_sha1_text)

    jsonDbGet connection_data "/api/patterns-timeline" SqlRead.apiPatternOccurrenceTimeline

    jsonDbGet connection_data "/api/patterns" SqlRead.apiPatterns

    jsonDbGet connection_data "/api/list-failure-modes" SqlRead.apiListFailureModes

    jsonDbGet connection_data "/api/patterns-presumed-stable-branches" SqlRead.api_patterns_presumed_stable_branches

    jsonDbGet1 connection_data "/api/patterns-presumed-stable-branches" "branches" SqlRead.api_patterns_branch_filtered

    jsonDbGet1Either connection_data "/api/single-build-info" "build_id" $ SqlUpdate.get_build_info (AuthConfig.personal_access_token github_config) . Builds.NewBuildNumber

    S.get "/api/best-build-match" $ do
      build_id <- S.param "build_id"
      S.json =<< liftIO (SqlRead.get_best_build_match connection_data $ Builds.NewBuildNumber build_id)

    jsonDbGet1 connection_data "/api/build-pattern-matches" "build_id" $ SqlRead.get_build_pattern_matches . Builds.NewBuildNumber

    jsonDbGet1 connection_data "/api/best-pattern-matches" "pattern_id" $ SqlRead.get_best_pattern_matches . ScanPatterns.PatternId

    jsonDbGet1 connection_data "/api/pattern-matches" "pattern_id" $ SqlRead.get_pattern_matches . ScanPatterns.PatternId

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file $ static_base </> "images/favicon.ico"

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file $ static_base </> "index.html"




