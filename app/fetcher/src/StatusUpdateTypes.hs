module StatusUpdateTypes where

import           Data.Text    (Text)

import qualified CommitBuilds
import qualified SqlUpdate
import qualified WebApi


data CommitPageInfo = CommitPageInfo {
    _revision_builds  :: [CommitBuilds.CommitBuild]
  , _unmatched_builds :: [WebApi.UnmatchedBuild]
  }


data BuildSummaryStats = NewBuildSummaryStats {
    flaky_count                 :: Int -- ^ flaky count
  , _upstream_breakages_info    :: SqlUpdate.UpstreamBreakagesInfo
  , total_circleci_fail_joblist :: [Text]
  }
