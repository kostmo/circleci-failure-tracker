{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module WebApi where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

import qualified Builds
import qualified JsonUtils


data JsonEither a b = JsonEither {
    _success :: Bool
  , _error   :: Maybe JsonUtils.ErrorDetails
  , _payload :: Maybe b
  } deriving Generic


instance (ToJSON a, ToJSON b) => ToJSON (JsonEither a b) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


toJsonEither :: (JsonUtils.WithErrorDetails a, ToJSON a, ToJSON b) => Either a b -> JsonEither a b
toJsonEither input = case input of
  Right x -> JsonEither True Nothing (Just x)
  Left x  -> JsonEither False (Just $ JsonUtils.getDetails x) Nothing


data ApiResponse a = ApiResponse {
    rows :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (ApiResponse a)


data BuildNumberRecord = BuildNumberRecord {
    _build_number :: Builds.BuildNumber
  } deriving Generic

instance ToJSON BuildNumberRecord where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data UnmatchedBuild = UnmatchedBuild {
    _build     :: Builds.BuildNumber
  , _step_name :: Text
  , _queued_at :: UTCTime
  , _job_name  :: Text
  , _branch    :: Text
  } deriving Generic

instance ToJSON UnmatchedBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data BuildBranchRecord = BuildBranchRecord {
    _build_number :: Builds.BuildNumber
  , _branch       :: Text
  } deriving Generic

instance ToJSON BuildBranchRecord where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data JobApiRecord = JobApiRecord {
    _name :: Text
  , _data :: [Int]
  } deriving Generic

instance ToJSON JobApiRecord where
  toJSON = genericToJSON JsonUtils.dropUnderscore


data PieSliceApiRecord = PieSliceApiRecord {
    _name :: Text
  , _y    :: Integer
  } deriving Generic

instance ToJSON PieSliceApiRecord where
  toJSON = genericToJSON JsonUtils.dropUnderscore
