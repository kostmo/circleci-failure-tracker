{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostedStatuses where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified JsonUtils


data PostedStatus = PostedStatus {
    _sha1        :: Text
  , _description :: Text
  , _state       :: Text
  , _created_at  :: UTCTime
  } deriving Generic

instance ToJSON PostedStatus where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data PostedStatusAggregate = PostedStatusAggregate {
    _sha1          :: Text
  , _count         :: Int
  , _last_time     :: UTCTime
  , _time_interval :: Double
  } deriving Generic

instance ToJSON PostedStatusAggregate where
  toJSON = genericToJSON JsonUtils.dropUnderscore
