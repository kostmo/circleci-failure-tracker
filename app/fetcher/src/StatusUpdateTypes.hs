module StatusUpdateTypes where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (partition)
import qualified Data.Maybe          as Maybe
import           Data.Text           (Text)

import qualified Builds
import qualified CommitBuilds
import qualified MyUtils
import qualified SqlRead
import qualified SqlUpdate
import qualified WebApi


data FlakyBuildPartition = FlakyBuildPartition {
    flaky_builds    :: [CommitBuilds.BuildWithLogContext]
  , nonflaky_builds :: [CommitBuilds.BuildWithLogContext]
  }


partitionMatchedBuilds ::
     HashMap Text SqlRead.UpstreamBrokenJob
  -> [CommitBuilds.BuildWithLogContext]
  -> UpstreamBuildPartition
partitionMatchedBuilds pre_broken_jobs_map pattern_matched_builds =
  UpstreamBuildPartition paired_upstream_breakages flakiness_partition
  where

    paired_upstream_causes = map (MyUtils.derivePair $ (`HashMap.lookup` pre_broken_jobs_map) . get_job_name_from_build_with_log_context) pattern_matched_builds

    (upstream_breakages, non_upstream_breakages_raw) = partition (not . null . snd) paired_upstream_causes

    paired_upstream_breakages = Maybe.mapMaybe sequenceA upstream_breakages

    (nonupstream_flaky_breakages, nonupstream_nonflaky_breakages) =
      partition flakinessPredicate $ map fst non_upstream_breakages_raw

    flakiness_partition = FlakyBuildPartition
      nonupstream_flaky_breakages
      nonupstream_nonflaky_breakages


data UpstreamBuildPartition = UpstreamBuildPartition {
    upstream    :: [(CommitBuilds.BuildWithLogContext, SqlRead.UpstreamBrokenJob)]
  , nonupstream :: FlakyBuildPartition
  }


data CommitPageInfo = CommitPageInfo {
    pattern_matched_builds :: UpstreamBuildPartition
  , unmatched_builds       :: [WebApi.UnmatchedBuild]
  }


data BuildSummaryStats = NewBuildSummaryStats {
    _upstream_breakages_info    :: SqlUpdate.UpstreamBreakagesInfo
  , total_circleci_fail_joblist :: [Text]
  }


flakinessPredicate = CommitBuilds._is_flaky . CommitBuilds._failure_mode . CommitBuilds.commit_build


get_job_name_from_build_with_log_context (CommitBuilds.BuildWithLogContext (CommitBuilds.NewCommitBuild (Builds.StorableBuild _ build_obj) _ _ _) _) = Builds.job_name build_obj
