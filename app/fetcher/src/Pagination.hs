module Pagination where

import           GHC.Int     (Int64)

import qualified Builds
import qualified WeeklyStats


data TimelineParms = TimelineParms {
    should_suppress_scheduled_builds :: Bool
  , offset_mode                      :: ParentOffsetMode
  }


data ParentOffsetMode =
    FixedAndOffset OffsetLimit
  | CommitIndices (WeeklyStats.InclusiveNumericBounds Int64)


data OffsetMode =
    Count Int
  | Commit Builds.RawCommit
  deriving Show


data OffsetLimit = OffsetLimit {
    offset :: OffsetMode
  , limit  :: Int
  }
