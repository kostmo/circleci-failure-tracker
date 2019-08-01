{-# LANGUAGE OverloadedStrings #-}

module SqlWrite where

import           Control.Applicative               ((<|>))
import           Control.Exception                 (throwIO)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (ExceptT (ExceptT), except,
                                                    runExceptT)
import           Data.Bifunctor                    (first)
import qualified Data.ByteString.Char8             as BS
import           Data.Either.Utils                 (maybeToEither)
import           Data.Foldable                     (for_)
import qualified Data.Maybe                        as Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Time.Format                  (defaultTimeLocale,
                                                    formatTime,
                                                    rfc822DateFormat)
import           Data.Traversable                  (for)
import           Data.Tuple                        (swap)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Errors
import           GHC.Int                           (Int64)
import qualified Network.OAuth.OAuth2              as OAuth2
import qualified Safe

import qualified ApiPost
import qualified AuthStages
import qualified Breakages
import qualified Builds
import qualified Commits
import qualified DbHelpers
import qualified GithubApiFetch
import qualified GitHubRecords
import qualified ScanPatterns
import qualified ScanRecords
import qualified SqlRead


circleCIProviderIndex = 3


sqlInsertUniversalBuild :: Query
sqlInsertUniversalBuild = "INSERT INTO universal_builds(provider, build_number, build_namespace, succeeded, commit_sha1) VALUES(?,?,?,?,?) ON CONFLICT ON CONSTRAINT universal_builds_build_number_build_namespace_provider_key DO UPDATE SET build_number = excluded.build_number RETURNING id;"


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

    insertion_sql = "INSERT INTO commit_metadata(sha1, message, tree_sha1, author_name, author_email, author_date, committer_name, committer_email, committer_date) VALUES(?,?,?,?,?,?,?,?,?);"

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


populateLatestMasterCommits ::
     Connection
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> IO (Either Text (Int64, Int64))
populateLatestMasterCommits conn access_token owned_repo = do

  maybe_latest_known_commit <- SqlRead.getLatestKnownMasterCommit conn

  runExceptT $ do

    -- The admin must manually populate the first several thousand commits,
    -- as these would be inefficient to fetch from the GitHub API.
    latest_known_commit <- except $ maybeToEither "Database has no commits" maybe_latest_known_commit

    fetched_commits_newest_first <- ExceptT $ first TL.toStrict <$> GithubApiFetch.getCommits
      access_token
      owned_repo
      Builds.masterName
      latest_known_commit

    let fetched_commits_oldest_first = reverse fetched_commits_newest_first

    commit_insertion_count <- ExceptT $ storeMasterCommits conn $ map GitHubRecords.extractCommitSha fetched_commits_oldest_first
    liftIO $ putStrLn $ unwords [
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
    insertion_sql = "INSERT INTO ordered_master_commits(sha1) VALUES(?);"

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


-- | TODO we may only need the multiple-row version of this
--
-- Currently, we handle de-duplication of records on the client side.
-- This should be handled in the INSERT statement.
insertSingleUniversalBuild ::
     Connection
  -> Builds.UniversalBuild
  -> IO (DbHelpers.WithId Builds.UniversalBuild)
insertSingleUniversalBuild conn uni_build@(Builds.UniversalBuild (Builds.NewBuildNumber provider_buildnum) provider_id build_namespace succeeded (Builds.RawCommit sha1)) = do
  [Only new_id] <- query conn sqlInsertUniversalBuild (provider_id, provider_buildnum, build_namespace, succeeded, sha1)
  return $ DbHelpers.WithId new_id uni_build


-- TODO This is the more efficient "bulk" operation, but needs to
-- handle constraint violations on individual rows.
--
-- for now, this function is only called from the standalone scanner application.
storeCircleCiBuildsList :: Connection -> [(Builds.Build, Bool)] -> IO Int64
storeCircleCiBuildsList conn builds_list = do
  universal_build_insertion_output_rows <- returning conn sqlInsertUniversalBuild $ map input_f universal_builds

  let zipped_output1 = zipWith (\(Only row_id) ubuild -> DbHelpers.WithId row_id ubuild) universal_build_insertion_output_rows universal_builds
      zipped_output2 = zipWith Builds.StorableBuild zipped_output1 $ map fst builds_list

  storeBuildsList conn zipped_output2

  where
    mk_ubuild (b, succeeded) = Builds.UniversalBuild (Builds.build_id b) circleCIProviderIndex "" succeeded (Builds.vcs_revision b)
    universal_builds = map mk_ubuild builds_list

    input_f (Builds.UniversalBuild (Builds.NewBuildNumber provider_buildnum) provider_id build_namespace succeeded (Builds.RawCommit sha1)) = (provider_id, provider_buildnum, build_namespace, succeeded, sha1)


-- | This is idempotent; builds that are already present will not be overwritten
storeBuildsList :: Connection -> [Builds.StorableBuild] -> IO Int64
storeBuildsList conn builds_list =
  executeMany conn sql $ map f builds_list
  where
    f (Builds.StorableBuild universal_build rbuild) =
      (queued_at_string, jobname, branch, DbHelpers.db_id universal_build)
      where
        queued_at_string = T.pack $ formatTime defaultTimeLocale rfc822DateFormat queuedat
        (Builds.NewBuild _ _ queuedat jobname branch) = rbuild

    sql = "INSERT INTO builds(queued_at, job_name, branch, global_build_num) VALUES(?,?,?,?) ON CONFLICT DO NOTHING;"


storeMatches ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> Builds.BuildNumber
  -> [ScanPatterns.ScanMatch]
  -> IO Int64
storeMatches scan_resources (Builds.NewBuildStepId build_step_id) _build_num scoped_matches =
  executeMany conn insertion_sql $ map to_tuple scoped_matches

  where
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources
    scan_id = ScanRecords.scan_id scan_resources

    to_tuple match = (
        scan_id
      , build_step_id
      , DbHelpers.db_id $ ScanPatterns.scanned_pattern match
      , ScanPatterns.line_number match_deets
      , ScanPatterns.line_text match_deets
      , ScanPatterns.start $ ScanPatterns.span match_deets
      , ScanPatterns.end $ ScanPatterns.span match_deets
      )
      where
        match_deets = ScanPatterns.match_details match

    insertion_sql = "INSERT INTO matches(scan_id, build_step, pattern, line_number, line_text, span_start, span_end) VALUES(?,?,?,?,?,?,?);"


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
    sql_insert = "INSERT INTO ci_providers(hostname) VALUES(?) RETURNING id;"


getAndStoreCIProviders ::
     DbHelpers.DbConnectionData
  -> [(String, a)]
  -> IO [(a, DbHelpers.WithId String)]
getAndStoreCIProviders conn_data failed_statuses_by_hostname = do
  conn <- DbHelpers.get_connection conn_data
  mapM (traverse (insertSingleCIProvider conn) . swap) failed_statuses_by_hostname


insert_posted_github_status ::
     DbHelpers.DbConnectionData
  -> Builds.RawCommit
  -> DbHelpers.OwnerAndRepo
  -> ApiPost.StatusPostResult
  -> IO Int64
insert_posted_github_status conn_data (Builds.RawCommit git_sha1) (DbHelpers.OwnerAndRepo owner repo) (ApiPost.StatusPostResult id url state desc target_url context created_at updated_at) = do
  conn <- DbHelpers.get_connection conn_data
  [Only pattern_id] <- query conn sql (id, git_sha1, owner, repo, url, state, desc, target_url, context, created_at, updated_at)
  return pattern_id
  where
    sql = "INSERT INTO created_github_statuses(id, sha1, project, repo, url, state, description, target_url, context, created_at, updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?) RETURNING id;"


add_pattern_tag ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Text
  -> IO (Either Text Int64)
add_pattern_tag conn_data (ScanPatterns.PatternId pattern_id) tag = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (pattern_id, tag)
  where
    sql = "INSERT INTO pattern_tags(pattern, tag) VALUES(?,?);"


remove_pattern_tag ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Text
  -> IO (Either Text Int64)
remove_pattern_tag conn_data (ScanPatterns.PatternId pattern_id) tag = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (pattern_id, tag)
  where
    sql = "DELETE FROM pattern_tags WHERE pattern = ? AND tag = ?;"


deleteCodeBreakage ::
     DbHelpers.DbConnectionData
  -> Int64
  -> IO (Either Text Int64)
deleteCodeBreakage conn_data cause_id = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (Only cause_id)
  where
    sql = "DELETE FROM code_breakage_cause WHERE id = ?;"


deleteCodeBreakageJob ::
     DbHelpers.DbConnectionData
  -> Int64
  -> Text
  -> IO (Either Text Int64)
deleteCodeBreakageJob conn_data cause_id job = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (cause_id, job)
  where
    sql = "DELETE FROM code_breakage_affected_jobs WHERE cause = ? AND job = ?;"


updateCodeBreakageDescription ::
     DbHelpers.DbConnectionData
  -> Int64
  -> Text
  -> IO (Either Text Int64)
updateCodeBreakageDescription conn_data cause_id description = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (description, cause_id)
  where
    sql = "UPDATE code_breakage_cause SET description = ? WHERE id = ?;"


updateCodeBreakageMode ::
     Connection
  -> AuthStages.Username
  -> Int64 -- ^ cause
  -> Int64 -- ^ mode
  -> IO (Either Text Int64)
updateCodeBreakageMode conn (AuthStages.Username author) cause_id mode =
  Right <$> execute conn insertion_sql (cause_id, author, mode)
  where
    insertion_sql = "INSERT INTO master_failure_mode_attributions(cause_id, reporter, mode_id) VALUES(?,?,?);"


updatePatternDescription ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Text
  -> IO (Either Text Int64)
updatePatternDescription conn_data (ScanPatterns.PatternId pattern_id) description = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (description, pattern_id)
  where
    sql = "UPDATE patterns SET description = ? WHERE id = ?;"


updatePatternSpecificity ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Int
  -> IO (Either Text Int64)
updatePatternSpecificity conn_data (ScanPatterns.PatternId pattern_id) specificity = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (specificity, pattern_id)
  where
    sql = "UPDATE patterns SET specificity = ? WHERE id = ?;"


insertSinglePattern ::
     Connection
  -> Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> IO Int64
insertSinglePattern conn either_pattern = do

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
    pattern_text = ScanPatterns.pattern_text expression_obj
    is_regex = ScanPatterns.is_regex expression_obj

    (ScanPatterns.NewPattern expression_obj description tags applicable_steps specificity is_retired lines_from_end) = pattern_obj

    (pattern_obj, AuthStages.Username author, maybe_timestamp, maybe_id) = case either_pattern of
      Left (patt_obj, username) -> (patt_obj, username, Nothing, Nothing)
      Right (DbHelpers.WithAuthorship auth created_time (DbHelpers.WithId _record_id patt_obj)) -> (patt_obj, AuthStages.Username auth, Just created_time, Nothing)

    has_nondeterminisic_values = case expression_obj of
      ScanPatterns.RegularExpression _ has_nondeterministic -> has_nondeterministic
      ScanPatterns.LiteralExpression _                       -> False

    pattern_insertion_sql = "INSERT INTO patterns(regex, expression, description, is_retired, has_nondeterministic_values, specificity, lines_from_end) VALUES(?,?,?,?,?,?,?) RETURNING id;"

    pattern_insertion_with_id_sql = "INSERT INTO patterns(id, regex, expression, description, is_retired, has_nondeterministic_values, specificity, lines_from_end) VALUES(?,?,?,?,?,?,?,?) RETURNING id;"

    tag_insertion_sql = "INSERT INTO pattern_tags(tag, pattern) VALUES(?,?);"

    authorship_insertion_with_timestamp_sql = "INSERT INTO pattern_authorship(pattern, author, created) VALUES(?,?,?);"
    authorship_insertion_sql = "INSERT INTO pattern_authorship(pattern, author) VALUES(?,?);"

    applicable_step_insertion_sql = "INSERT INTO pattern_step_applicability(step_name, pattern) VALUES(?,?);"


restore_patterns ::
     DbHelpers.DbConnectionData
  -> [DbHelpers.WithAuthorship ScanPatterns.DbPattern]
  -> IO (Either Text [Int64])
restore_patterns conn_data pattern_list = do
  conn <- DbHelpers.get_connection conn_data
  eithers <- for pattern_list $ apiNewPattern conn . Right
  return $ sequenceA eithers


stepFailureToTuple ::
     (Builds.UniversalBuildId, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
  -> (Maybe Text, Bool, Int64)
stepFailureToTuple (Builds.UniversalBuildId universal_buildnum, visitation_result) = case visitation_result of
  Right _ -> (Nothing, False, universal_buildnum)
  Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure stepname mode)) -> let
    is_timeout = case mode of
      Builds.BuildTimeoutFailure              -> True
      Builds.ScannableFailure _failure_output -> False
    in (Just stepname, is_timeout, universal_buildnum)


populatePresumedStableBranches :: Connection -> [Text] -> IO Int64
populatePresumedStableBranches conn =
  executeMany conn sql . map Only
  where
    sql = "INSERT INTO presumed_stable_branches(branch) VALUES(?);"


storeLogInfo ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> ScanRecords.LogInfo
  -> IO Int64
storeLogInfo
    scan_resources
    (Builds.NewBuildStepId step_id)
    (ScanRecords.LogInfo byte_count line_count log_content modified_by_ansi_stripping) =

  execute conn sql (step_id, line_count, byte_count, log_content, modified_by_ansi_stripping)
  where
    sql = "INSERT INTO log_metadata(step, line_count, byte_count, content, modified_by_ansi_stripping) VALUES(?,?,?,?,?) ON CONFLICT (step) DO UPDATE SET line_count = EXCLUDED.line_count, byte_count = EXCLUDED.byte_count, content = EXCLUDED.content, modified_by_ansi_stripping = EXCLUDED.modified_by_ansi_stripping;"
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insertLatestPatternBuildScan ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> Int64
  -> IO ()
insertLatestPatternBuildScan
    scan_resources
    (Builds.NewBuildStepId step_id)
    pattern_id = do

  execute conn sql (ScanRecords.scan_id scan_resources, step_id, pattern_id)
  return ()

  where
    sql = "INSERT INTO scanned_patterns(scan, step_id, newest_pattern) VALUES(?,?,?);"
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insertBuildVisitation ::
     ScanRecords.ScanCatchupResources
  -> (DbHelpers.WithId Builds.UniversalBuild, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure)
  -> IO Builds.BuildStepId
insertBuildVisitation scan_resources (ubuild, visitation_result) = do

  [Only step_id] <- query conn sql $
    stepFailureToTuple (Builds.UniversalBuildId $ DbHelpers.db_id ubuild, visitation_result)
  return $ Builds.NewBuildStepId step_id
  where
    sql = "INSERT INTO build_steps(name, is_timeout, universal_build) VALUES(?,?,?) RETURNING id;"
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
    sql = "INSERT INTO scans(latest_pattern_id, initiator) VALUES(?,?) RETURNING id;"


apiCodeBreakageCauseInsert ::
     Connection
  -> Breakages.BreakageReport
  -> [Text] -- ^ job names
  -> IO (Either Text Int64)
apiCodeBreakageCauseInsert
    conn
    (Breakages.NewBreakageReport sha1 description (AuthStages.Username author_username))
    job_names =

  catchViolation catcher $ do

    [Only report_id] <- query conn insertion_sql (sha1, description, author_username)

    for_ job_names $ \job ->
      execute conn job_insertion_sql (job, report_id, author_username)

    return $ Right report_id

  where
    insertion_sql = "INSERT INTO code_breakage_cause(sha1, description, reporter) VALUES(?,?,?) RETURNING id;"
    job_insertion_sql = "INSERT INTO code_breakage_affected_jobs(job, cause, reporter) VALUES(?,?,?);"

    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


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
    insertion_sql = "INSERT INTO code_breakage_resolution(sha1, cause, reporter) VALUES(?,?,?) RETURNING id;"

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
  , pat_applicable_steps :: Maybe [String]
  , pat_lines_from_end   :: Maybe Int
  }


copyPattern ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> AuthStages.Username
  -> PatternFieldOverrides
  -> IO (Either Text Int64)
copyPattern conn_data pattern_id@(ScanPatterns.PatternId pat_id) username field_overrides = do

  conn <- DbHelpers.get_connection conn_data
  pattern_rows <- query conn sql $ Only pat_id

  runExceptT $ do

    p <- except $ maybeToEither (T.pack $ "Pattern with ID " ++ show pat_id ++ " not found.") $ Safe.headMay pattern_rows

    let (p_is_regex, p_has_nondeterministic_values, p_expression_text, p_description, p_tags_concatenated, p_steps_concatenated, p_specificity, p_maybe_lines_from_end) = p
        expression_text = Maybe.fromMaybe p_expression_text $ pat_expression field_overrides
        is_regex = Maybe.fromMaybe p_is_regex $ pat_is_regex field_overrides

        new_pattern = ScanPatterns.NewPattern
          (ScanPatterns.toMatchExpression is_regex expression_text p_has_nondeterministic_values)
          p_description
          (map T.pack $ DbHelpers.splitAggText p_tags_concatenated)
          (map T.pack $ Maybe.fromMaybe (DbHelpers.splitAggText p_steps_concatenated) $ pat_applicable_steps field_overrides)
          p_specificity
          False
          (pat_lines_from_end field_overrides <|> p_maybe_lines_from_end)

    ExceptT $ do
      new_id <- apiNewPattern conn $ Left (new_pattern, username)
      retirePattern conn pattern_id
      return new_id

  where
    sql = "SELECT regex, has_nondeterministic_values, expression, description, tags, steps, specificity, lines_from_end FROM patterns_augmented WHERE id = ?;"


apiNewPattern ::
     Connection
  -> Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> IO (Either Text Int64)
apiNewPattern conn new_pattern =

  catchViolation catcher $ do
    record_id <- insertSinglePattern conn new_pattern
    return $ Right record_id

  where
    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e
