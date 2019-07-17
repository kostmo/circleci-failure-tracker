{-# LANGUAGE DeriveGeneric #-}

module WeeklyStats where

import           Data.Aeson
import           Data.Time    (UTCTime)
import           GHC.Generics
import           GHC.Int      (Int64)

import qualified JsonUtils


data InclusiveNumericBounds a = InclusiveNumericBounds {
    min_bound :: a
  , max_bound :: a
  } deriving Generic

instance (ToJSON a) => ToJSON (InclusiveNumericBounds a)


-- | This holds two types of aggregations:
-- 1) whether each commit had *any* failure of the given type
-- 2) How *many* jobs experienced the failure of the given type for each commit.
--
-- Both of the above aggregations are then again summed by week.
data MasterWeeklyStats = MasterWeeklyStats {
    _commit_count          :: Int
  , _had_failure           :: Int
  , _had_idiopathic        :: Int
  , _had_timeout           :: Int
  , _had_known_broken      :: Int
  , _had_pattern_matched   :: Int
  , _had_flaky             :: Int

  , _failure_count         :: Int
  , _idiopathic_count      :: Int
  , _timeout_count         :: Int
  , _known_broken_count    :: Int
  , _pattern_matched_count :: Int
  , _flaky_count           :: Int

  , _week                  :: UTCTime
  , _commit_id_bound       :: InclusiveNumericBounds Int64
  } deriving Generic

instance ToJSON MasterWeeklyStats where
  toJSON = genericToJSON JsonUtils.dropUnderscore


