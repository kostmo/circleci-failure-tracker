module Pagination where

import qualified Builds


data OffsetMode = Count Int | Commit Builds.RawCommit
  deriving Show

data OffsetLimit = OffsetLimit {
    offset :: OffsetMode
  , limit  :: Int
  }
