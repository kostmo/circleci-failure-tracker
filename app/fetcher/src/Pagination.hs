module Pagination where

data OffsetLimit = OffsetLimit {
    offset :: Int
  , limit  :: Int
  }
