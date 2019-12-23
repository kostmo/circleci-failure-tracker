module StatusUpdateTypes where

import           Data.List    (partition)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Text    (Text)

import qualified Builds
import qualified CommitBuilds
import qualified SqlUpdate
import qualified WebApi


data FlakyBuildPartition = FlakyBuildPartition {
    flaky_builds    :: [CommitBuilds.BuildWithLogContext]
  , nonflaky_builds :: [CommitBuilds.BuildWithLogContext]
  }


partitionMatchedBuilds ::
     Set Text
  -> [CommitBuilds.BuildWithLogContext]
  -> UpstreamBuildPartition
partitionMatchedBuilds pre_broken_set pattern_matched_builds =
  UpstreamBuildPartition upstream_breakages flakiness_partition
  where

    flakiness_partition = FlakyBuildPartition
      nonupstream_flaky_breakages
      nonupstream_nonflaky_breakages

    (nonupstream_flaky_breakages, nonupstream_nonflaky_breakages) = partition flakinessPredicate non_upstream_breakages_raw


    upstream_predicate = (`Set.member` pre_broken_set) . get_job_name_from_build_with_log_context

    (upstream_breakages, non_upstream_breakages_raw) = partition upstream_predicate pattern_matched_builds


data UpstreamBuildPartition = UpstreamBuildPartition {
    upstream    :: [CommitBuilds.BuildWithLogContext]
  , nonupstream :: FlakyBuildPartition
  }


data CommitPageInfo = CommitPageInfo {
    pattern_matched_builds :: UpstreamBuildPartition
  , unmatched_builds       :: [WebApi.UnmatchedBuild]
  }


data BuildSummaryStats = NewBuildSummaryStats {
    flaky_count                 :: Int -- ^ flaky count
  , _upstream_breakages_info    :: SqlUpdate.UpstreamBreakagesInfo
  , total_circleci_fail_joblist :: [Text]
  }


flakinessPredicate = CommitBuilds._is_flaky . CommitBuilds._failure_mode . CommitBuilds.commit_build


deriveSummaryStats ::
     CommitPageInfo
  -> SqlUpdate.UpstreamBreakagesInfo
  -> [Text]
  -> BuildSummaryStats
deriveSummaryStats
    commit_page_info
    upstream_breakages_info
    circleci_fail_joblist =

  NewBuildSummaryStats
    flaky_count
    upstream_breakages_info
    circleci_fail_joblist

  where
  flaky_count = length $ flaky_builds $ nonupstream $ pattern_matched_builds commit_page_info


get_job_name_from_build_with_log_context (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild _ build_obj) _ _ _) _) = Builds.job_name build_obj
