{-# LANGUAGE OverloadedStrings #-}

module SqlWrite where

import           Control.Applicative               ((<|>))
import           Control.Exception                 (throwIO)
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
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Errors
import           GHC.Int                           (Int64)
import qualified Network.OAuth.OAuth2              as OAuth2
import qualified Safe

import qualified ApiPost
import qualified AuthStages
import qualified Breakages
import qualified Breakages2
import qualified Builds
import qualified Commits
import qualified DbHelpers
import qualified GithubApiFetch
import qualified GitHubRecords
import qualified ScanPatterns
import qualified ScanRecords
import qualified SqlRead


buildToTuple :: Builds.Build -> (Int64, Text, Text, Text, Text)
buildToTuple (Builds.NewBuild (Builds.NewBuildNumber build_num) (Builds.RawCommit vcs_rev) queuedat jobname branch) =
  (build_num, vcs_rev, queued_at_string, jobname, branch)
  where
    queued_at_string = T.pack $ formatTime defaultTimeLocale rfc822DateFormat queuedat


storeCommitMetadata ::
     DbHelpers.DbConnectionData
  -> [Commits.CommitMetadata]
  -> IO (Either Text Int64)
storeCommitMetadata conn_data commit_list = do
  conn <- DbHelpers.get_connection conn_data

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
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> IO (Either Text Int64)
populateLatestMasterCommits conn_data access_token owned_repo = do

  conn <- DbHelpers.get_connection conn_data

  maybe_latest_known_commit <- SqlRead.get_latest_known_master_commit conn

  runExceptT $ do

    -- The admin must manually populate the first several thousand commits,
    -- as these would be inefficient to fetch from the GitHub API.
    latest_known_commit <- except $ maybeToEither "Database has no commits" maybe_latest_known_commit

    fetched_commits_newest_first <- ExceptT $ first TL.toStrict <$> GithubApiFetch.getCommits
      access_token
      owned_repo
      "master"
      latest_known_commit

    let fetched_commits_oldest_first = reverse fetched_commits_newest_first

    ExceptT $ do
      insertion_count <- storeMasterCommits conn_data $ map GitHubRecords._sha fetched_commits_oldest_first
      putStrLn $ "Inserted " ++ show insertion_count ++ " commits"
      return insertion_count


storeMasterCommits :: DbHelpers.DbConnectionData -> [Text] -> IO (Either Text Int64)
storeMasterCommits conn_data commit_list = do
  conn <- DbHelpers.get_connection conn_data

  catchViolation catcher $ do
    count <- executeMany conn insertion_sql $ map Only commit_list
    return $ Right count

  where
    insertion_sql = "INSERT INTO ordered_master_commits(sha1) VALUES(?);"

    catcher _ (UniqueViolation some_error) = return $ Left $
      "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                            = throwIO e


-- | This is idempotent; builds that are already present will not be overwritten
store_builds_list :: Connection -> [Builds.Build] -> IO Int64
store_builds_list conn builds_list =
  executeMany conn sql $ map buildToTuple builds_list
  where
    sql = "INSERT INTO builds(build_num, vcs_revision, queued_at, job_name, branch) VALUES(?,?,?,?,?) ON CONFLICT (build_num) DO NOTHING;"


store_matches ::
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> Builds.BuildNumber
  -> [ScanPatterns.ScanMatch]
  -> IO Int64
store_matches scan_resources (Builds.NewBuildStepId build_step_id) _build_num scoped_matches =
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


insert_posted_github_status ::
     DbHelpers.DbConnectionData
  -> Text
  -> DbHelpers.OwnerAndRepo
  -> ApiPost.StatusPostResult
  -> IO Int64
insert_posted_github_status conn_data git_sha1 (DbHelpers.OwnerAndRepo owner repo) (ApiPost.StatusPostResult id url state desc target_url context created_at updated_at) = do
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


update_code_breakage_description ::
     DbHelpers.DbConnectionData
  -> Int64
  -> Text
  -> IO (Either Text Int64)
update_code_breakage_description conn_data cause_id description = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (description, cause_id)
  where
    sql = "UPDATE code_breakage_cause SET description = ? WHERE id = ?;"




update_pattern_description ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Text
  -> IO (Either Text Int64)
update_pattern_description conn_data (ScanPatterns.PatternId pattern_id) description = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (description, pattern_id)
  where
    sql = "UPDATE patterns SET description = ? WHERE id = ?;"


update_pattern_specificity ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> Int
  -> IO (Either Text Int64)
update_pattern_specificity conn_data (ScanPatterns.PatternId pattern_id) specificity = do
  conn <- DbHelpers.get_connection conn_data
  Right <$> execute conn sql (specificity, pattern_id)
  where
    sql = "UPDATE patterns SET specificity = ? WHERE id = ?;"


insert_single_pattern ::
     Connection
  -> Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> IO Int64
insert_single_pattern conn either_pattern = do

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
--      Right (DbHelpers.WithAuthorship auth created_time (DbHelpers.WithId record_id patt_obj)) -> (patt_obj, AuthStages.Username auth, Just created_time, Just record_id)
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
  eithers <- for pattern_list $ api_new_pattern conn . Right
  return $ sequenceA eithers


step_failure_to_tuple :: (Builds.BuildNumber, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure) -> (Int64, Maybe Text, Bool)
step_failure_to_tuple (Builds.NewBuildNumber buildnum, visitation_result) = case visitation_result of
  Right _ -> (buildnum, Nothing, False)
  Left (Builds.BuildWithStepFailure _build_obj (Builds.NewBuildStepFailure stepname mode)) -> let
    is_timeout = case mode of
      Builds.BuildTimeoutFailure              -> True
      Builds.ScannableFailure _failure_output -> False
    in (buildnum, Just stepname, is_timeout)


populate_presumed_stable_branches :: Connection -> [Text] -> IO Int64
populate_presumed_stable_branches conn =
  executeMany conn sql . map Only
  where
    sql = "INSERT INTO presumed_stable_branches(branch) VALUES(?);"


store_log_info :: ScanRecords.ScanCatchupResources -> Builds.BuildStepId -> ScanRecords.LogInfo -> IO Int64
store_log_info scan_resources (Builds.NewBuildStepId step_id) (ScanRecords.LogInfo byte_count line_count log_content) =
  execute conn sql (step_id, line_count, byte_count, log_content)
  where
    sql = "INSERT INTO log_metadata(step, line_count, byte_count, content) VALUES(?,?,?,?) ON CONFLICT (step) DO UPDATE SET line_count = EXCLUDED.line_count, byte_count = EXCLUDED.byte_count, content = EXCLUDED.content;"
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insert_latest_pattern_build_scan :: ScanRecords.ScanCatchupResources -> Builds.BuildNumber -> Int64 -> IO ()
insert_latest_pattern_build_scan scan_resources (Builds.NewBuildNumber build_number) pattern_id = do

  execute conn sql (ScanRecords.scan_id scan_resources, build_number, pattern_id)
  return ()

  where
    sql = "INSERT INTO scanned_patterns(scan, build, newest_pattern) VALUES(?,?,?);"
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insert_build_visitation :: ScanRecords.ScanCatchupResources -> (Builds.BuildNumber, Either Builds.BuildWithStepFailure ScanRecords.UnidentifiedBuildFailure) -> IO Builds.BuildStepId
insert_build_visitation scan_resources visitation = do

  [Only step_id] <- query conn sql $ step_failure_to_tuple visitation
  return $ Builds.NewBuildStepId step_id
  where
    sql = "INSERT INTO build_steps(build, name, is_timeout) VALUES(?,?,?) RETURNING id;"
    conn = ScanRecords.db_conn $ ScanRecords.fetching scan_resources


insert_scan_id :: Connection -> Maybe AuthStages.Username -> ScanPatterns.PatternId -> IO Int64
insert_scan_id conn maybe_initiator (ScanPatterns.PatternId pattern_id)  = do
  [Only pattern_id] <- query conn sql (pattern_id, inititator)
  return pattern_id
  where
    inititator = fmap (\(AuthStages.Username x) -> x) maybe_initiator
    sql = "INSERT INTO scans(latest_pattern_id, initiator) VALUES(?,?) RETURNING id;"


api_new_breakage_report ::
     DbHelpers.DbConnectionData
  -> Breakages.BreakageReport
  -> IO (Either Text Int64)
api_new_breakage_report
    conn_data
    (Breakages.NewBreakageReport (Builds.NewBuildStepId build_step_id) implicated_rev is_broken notes (AuthStages.Username author_username)) = do

  conn <- DbHelpers.get_connection conn_data
  catchViolation catcher $ do
    [Only report_id] <- query conn insertion_sql (build_step_id, author_username, is_broken, implicated_rev, notes)
    return $ Right report_id

  where
    insertion_sql = "INSERT INTO broken_build_reports(build_step, reporter, is_broken, implicated_revision, notes) VALUES(?,?,?,?,?) RETURNING id;"

    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


api_code_breakage_cause_insert ::
     DbHelpers.DbConnectionData
  -> Breakages2.BreakageReport
  -> [Text] -- ^ job names
  -> IO (Either Text Int64)
api_code_breakage_cause_insert
    conn_data
    (Breakages2.NewBreakageReport sha1 description (AuthStages.Username author_username))
    job_names = do

  conn <- DbHelpers.get_connection conn_data
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


api_code_breakage_resolution_insert ::
     DbHelpers.DbConnectionData
  -> Breakages2.ResolutionReport
  -> IO (Either Text Int64)
api_code_breakage_resolution_insert
    conn_data
    (Breakages2.NewResolutionReport sha1 cause_id (AuthStages.Username author_username)) = do

  conn <- DbHelpers.get_connection conn_data

  -- TODO: Ensure that the commit index of the resolution is strictly higher
  -- than the commit index of the cause

  catchViolation catcher $ do

    [Only report_id] <- query conn insertion_sql (sha1, cause_id, author_username)
    return $ Right report_id

  where
    insertion_sql = "INSERT INTO code_breakage_resolution(sha1, cause, reporter) VALUES(?,?,?) RETURNING id;"

    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e


retire_pattern ::
     Connection
  -> ScanPatterns.PatternId
  -> IO ()
retire_pattern conn (ScanPatterns.PatternId pattern_id) = do
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


copy_pattern ::
     DbHelpers.DbConnectionData
  -> ScanPatterns.PatternId
  -> AuthStages.Username
  -> PatternFieldOverrides
  -> IO (Either Text Int64)
copy_pattern conn_data pattern_id@(ScanPatterns.PatternId pat_id) username field_overrides = do

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
          (map T.pack $ SqlRead.splitAggText p_tags_concatenated)
          (map T.pack $ Maybe.fromMaybe (SqlRead.splitAggText p_steps_concatenated) $ pat_applicable_steps field_overrides)
          p_specificity
          False
          (pat_lines_from_end field_overrides <|> p_maybe_lines_from_end)

    ExceptT $ do
      new_id <- api_new_pattern conn $ Left (new_pattern, username)
      retire_pattern conn pattern_id
      return new_id

  where
    sql = "SELECT regex, has_nondeterministic_values, expression, description, tags, steps, specificity, lines_from_end FROM patterns_augmented WHERE id = ?;"


api_new_pattern ::
     Connection
  -> Either (ScanPatterns.Pattern, AuthStages.Username) (DbHelpers.WithAuthorship ScanPatterns.DbPattern)
  -> IO (Either Text Int64)
api_new_pattern conn new_pattern =

  catchViolation catcher $ do
    record_id <- insert_single_pattern conn new_pattern
    return $ Right record_id

  where
    catcher _ (UniqueViolation some_error) = return $ Left $ "Insertion error: " <> T.pack (BS.unpack some_error)
    catcher e _                                  = throwIO e
