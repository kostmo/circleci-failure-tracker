module Pagination where

import           GHC.Int     (Int64)

import qualified Builds
import qualified WeeklyStats


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
