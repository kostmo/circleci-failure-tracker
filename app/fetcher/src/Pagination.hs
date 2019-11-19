module Pagination where

import           GHC.Int   (Int64)

import qualified Builds
import qualified DbHelpers


data ColumnFilteringOptions = ColumnFilteringOptions {
    should_suppress_scheduled_builds         :: Bool
  , should_suppress_fully_successful_columns :: Maybe Int
  }


data TimelineParms = TimelineParms {
    column_filtering :: ColumnFilteringOptions
  , offset_mode      :: ParentOffsetMode
  }


data ParentOffsetMode =
    FixedAndOffset OffsetLimit
  | CommitIndices (DbHelpers.InclusiveNumericBounds Int64)


data OffsetMode =
    Count Int
  | Commit Builds.RawCommit
  deriving Show


data OffsetLimit = OffsetLimit {
    offset :: OffsetMode
  , limit  :: Int
  }
