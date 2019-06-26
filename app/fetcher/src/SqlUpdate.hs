{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module SqlUpdate where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.Aeson
import           Data.Coerce                (coerce)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Network.OAuth.OAuth2       as OAuth2

import qualified Builds
import qualified CommitBuilds
import qualified Constants
import qualified DbHelpers
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite


data CommitInfoCounts = NewCommitInfoCounts {
    _failed_build_count  :: Int
  , _timeout_count       :: Int
  , _matched_build_count :: Int
  , _code_breakage_count :: Int
  , _flaky_build_count   :: Int
  , _known_broken_count  :: Int
  } deriving Generic

instance ToJSON CommitInfoCounts where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data CommitInfo = NewCommitInfo {
    _breakages :: [DbHelpers.WithId SqlRead.CodeBreakage]
  , _counts    :: CommitInfoCounts
  } deriving Generic

instance ToJSON CommitInfo where
  toJSON = genericToJSON JsonUtils.dropUnderscore



count_revision_builds ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> GitRev.GitSha1
  -> IO (Either Text CommitInfo)
count_revision_builds conn_data access_token git_revision = do
  conn <- DbHelpers.get_connection conn_data
  [Only failed_count] <- query conn failed_count_sql only_commit
  [Only matched_count] <- query conn matched_count_sql only_commit
  [Only timeout_count] <- query conn timeout_count_sql only_commit
  [Only reported_count] <- query conn reported_broken_count_sql only_commit

  revision_builds <- SqlRead.get_revision_builds conn_data git_revision
  flaky_pattern_ids <- SqlRead.get_flaky_pattern_ids conn

  let is_flaky = (`Set.member` flaky_pattern_ids) . coerce . MatchOccurrences._pattern_id . CommitBuilds._match
      flaky_builds = filter is_flaky revision_builds
      flaky_build_count = length flaky_builds

  runExceptT $ do

    breakages <- ExceptT $ findKnownBuildBreakages conn_data access_token (DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name) $ Builds.RawCommit sha1
    let all_broken_jobs = Set.unions $ map (SqlRead._jobs . DbHelpers.record) breakages

    [Only known_broken_count] <- liftIO $ query conn known_broken_count_sql (sha1, In $ Set.toAscList all_broken_jobs)
    return $ NewCommitInfo breakages $ NewCommitInfoCounts
      failed_count
      timeout_count
      matched_count
      reported_count
      flaky_build_count
      known_broken_count

  where
    sha1 = GitRev.sha1 git_revision
    only_commit = Only sha1

    timeout_count_sql = "SELECT COUNT(*) FROM builds_join_steps WHERE vcs_revision = ? AND is_timeout;"
    failed_count_sql = "SELECT COUNT(*) FROM builds WHERE vcs_revision = ?;"
    matched_count_sql = "SELECT COUNT(*) FROM best_pattern_match_augmented_builds WHERE vcs_revision = ?;"
    reported_broken_count_sql = "SELECT COUNT(*) FROM builds_with_reports WHERE vcs_revision = ? AND is_broken;"
    known_broken_count_sql = "SELECT COUNT(*) FROM builds WHERE vcs_revision = ? AND job_name IN ?;"





-- | Find known build breakages applicable to the merge base
-- of this PR commit
findKnownBuildBreakages ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.RawCommit
  -> IO (Either Text [DbHelpers.WithId SqlRead.CodeBreakage])
findKnownBuildBreakages db_connection_data access_token owned_repo sha1 =

  runExceptT $ do

    -- First, ensure knowledge of "master" branch lineage
    -- is up-to-date
    ExceptT $ SqlWrite.populate_latest_master_commits db_connection_data access_token owned_repo

    -- Second, find which "master" commit is the most recent
    -- ancestor of the given PR commit.
    nearest_ancestor <- ExceptT $ SqlRead.find_master_ancestor db_connection_data access_token owned_repo sha1

    -- Third, find whether that commit is within the
    -- [start, end) span of any known breakages
    ExceptT $ SqlRead.get_spanning_breakages db_connection_data nearest_ancestor
