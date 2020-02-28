module StatusUpdateTypes where

import           Data.List       (partition)
import           Data.Text       (Text)
import           Debug.Trace     (trace)

import qualified CommitBuilds
import qualified Sql.Read        as SqlRead
import qualified Sql.Update      as SqlUpdate
import qualified UnmatchedBuilds


class Partition a where
  count :: a -> Int


instance Partition [a] where
  count = length


type CommitBuildWrapperTuple = (CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, CommitBuilds.BuildWithLogContext)


data CommitPageInfo = NewCommitPageInfo {
    upstream_builds :: [(CommitBuilds.CommitBuildWrapper SqlRead.CommitBuildSupplementalPayload, SqlRead.UpstreamBrokenJob)]
  , nonupstream_builds       :: NonUpstreamBuildPartition
  }


data NonUpstreamBuildPartition = NewNonUpstreamBuildPartition {
    pattern_matched_builds :: FlakyBuildPartition CommitBuildWrapperTuple
  , unmatched_builds       :: [UnmatchedBuilds.UnmatchedBuild]
  }


data FlakyBuildPartition a = NewFlakyBuildPartition {
    tentatively_flaky_builds :: TentativeFlakyBuilds a
  , nonflaky_builds          :: NonFlakyBuilds a
  , confirmed_flaky_builds   :: [a]
  }


data TentativeFlakyBuilds a = NewTentativeFlakyBuilds {
    tentative_flaky_triggered_reruns   :: [a]
  , tentative_flaky_untriggered_reruns :: [a]
  }


data NonFlakyBuilds a = NewNonFlakyBuilds {
    nonflaky_by_pattern                :: [a]
  , nonflaky_by_empirical_confirmation :: [a]
  }


instance Partition (TentativeFlakyBuilds a) where
  count x = sum $ map (\f -> length $ f x) field_extractors
    where
      field_extractors = [
          tentative_flaky_triggered_reruns
        , tentative_flaky_untriggered_reruns
        ]


instance Partition (NonFlakyBuilds a) where
  count x = sum $ map (\f -> length $ f x) field_extractors
    where
      field_extractors = [
          nonflaky_by_pattern
        , nonflaky_by_empirical_confirmation
        ]


partitionMatchedBuilds ::
     [CommitBuildWrapperTuple]
  -> FlakyBuildPartition CommitBuildWrapperTuple
partitionMatchedBuilds pattern_matched_builds =
  NewFlakyBuildPartition
    tentative_flaky_builds_partition
    nonflaky_builds_partition
    confirmed_flaky_breakages

  where
    tentative_flaky_builds_partition = NewTentativeFlakyBuilds rerun_was_triggered_breakages rerun_not_triggered_breakages

    nonflaky_builds_partition_foo = NewNonFlakyBuilds nonupstream_nonflaky_breakages negatively_confirmed_flaky_breakages



    nonflaky_builds_partition = trace (unwords ["nonupstream_tentatively_flaky_breakages count:", show $ length nonupstream_tentatively_flaky_breakages, "nonupstream_nonflaky_breakages:", show $ length nonupstream_nonflaky_breakages]) nonflaky_builds_partition_foo






    -- Best pattern match is clasified as flaky
    tentative_flakiness_predicate = CommitBuilds._is_flaky . CommitBuilds._failure_mode . CommitBuilds._commit_build . fst



    (nonupstream_tentatively_flaky_breakages, nonupstream_nonflaky_breakages) =
      partition tentative_flakiness_predicate pattern_matched_builds


    has_completed_rerun_predicate = SqlRead.has_completed_rerun . CommitBuilds._supplemental . fst

    (completed_rerun_flaky_breakages, not_completed_rerun_flaky_breakages) =
      partition has_completed_rerun_predicate nonupstream_tentatively_flaky_breakages



    has_triggered_rerun_predicate = SqlRead.has_triggered_rebuild . CommitBuilds._supplemental . fst

    (rerun_was_triggered_breakages, rerun_not_triggered_breakages) =
      partition has_triggered_rerun_predicate not_completed_rerun_flaky_breakages



    flakiness_confirmed_predicate = SqlRead.is_empirically_determined_flaky . CommitBuilds._supplemental . fst

    (confirmed_flaky_breakages, negatively_confirmed_flaky_breakages) =
      partition flakiness_confirmed_predicate completed_rerun_flaky_breakages



data BuildSummaryStats = NewBuildSummaryStats {
    _upstream_breakages_info    :: SqlUpdate.UpstreamBreakagesInfo
  , total_circleci_fail_joblist :: [Text]
  }
