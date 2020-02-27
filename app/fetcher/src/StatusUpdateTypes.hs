module StatusUpdateTypes where

import           Data.List       (partition)
import           Data.Text       (Text)

import qualified Builds
import qualified CommitBuilds
import qualified Sql.Read        as SqlRead
import qualified Sql.Update      as SqlUpdate
import qualified UnmatchedBuilds


data CommitPageInfo = NewCommitPageInfo {
    upstream_builds :: [(CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, SqlRead.UpstreamBrokenJob)]
  , nonupstream_builds       :: NonUpstreamBuildPartition
  }


data NonUpstreamBuildPartition = NewNonUpstreamBuildPartition {
    pattern_matched_builds :: FlakyBuildPartition
  , unmatched_builds       :: [UnmatchedBuilds.UnmatchedBuild]
  }


data FlakyBuildPartition = NewFlakyBuildPartition {
    flaky_builds    :: [(CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, CommitBuilds.BuildWithLogContext)]
  , nonflaky_builds :: [(CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, CommitBuilds.BuildWithLogContext)]
  }


flakinessPredicate = CommitBuilds._is_flaky . CommitBuilds._failure_mode . CommitBuilds._commit_build


partitionMatchedBuilds ::
     [(CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, CommitBuilds.BuildWithLogContext)]
  -> FlakyBuildPartition
partitionMatchedBuilds pattern_matched_builds =
  NewFlakyBuildPartition nonupstream_flaky_breakages nonupstream_nonflaky_breakages

  where
    (nonupstream_flaky_breakages, nonupstream_nonflaky_breakages) =
      partition (flakinessPredicate . fst) pattern_matched_builds


data BuildSummaryStats = NewBuildSummaryStats {
    _upstream_breakages_info    :: SqlUpdate.UpstreamBreakagesInfo
  , total_circleci_fail_joblist :: [Text]
  }


getJobNameFromBuildWithLogContext (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild _ build_obj) _ _ _) _) = Builds.job_name build_obj
