{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module WebApi where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified Builds
import           JsonUtils    (WithErrorMessage, dropUnderscore, getMessage)


data ErrorDetails a = ErrorDetails {
    _message :: Text
  , _details :: a
  } deriving Generic

instance (ToJSON a) => ToJSON (ErrorDetails a) where
  toJSON = genericToJSON dropUnderscore


data JsonEither a b = JsonEither {
    _success :: Bool
  , _error   :: Maybe (ErrorDetails a)
  , _payload :: Maybe b
  } deriving Generic


instance (ToJSON a, ToJSON b) => ToJSON (JsonEither a b) where
  toJSON = genericToJSON dropUnderscore


toJsonEither :: (WithErrorMessage a, ToJSON a, ToJSON b) => Either a b -> JsonEither a b
toJsonEither input = case input of
  Right x -> JsonEither True Nothing (Just x)
  Left x  -> JsonEither False (Just $ ErrorDetails (getMessage x) x) Nothing


data ApiResponse a = ApiResponse {
    rows :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (ApiResponse a)


data BuildNumberRecord = BuildNumberRecord {
    _build_number :: Builds.BuildNumber
  } deriving Generic

instance ToJSON BuildNumberRecord where
  toJSON = genericToJSON dropUnderscore


data BuildBranchRecord = BuildBranchRecord {
    _build_number :: Builds.BuildNumber
  , _branch       :: Text
  } deriving Generic

instance ToJSON BuildBranchRecord where
  toJSON = genericToJSON dropUnderscore


data JobApiRecord = JobApiRecord {
    _name :: Text
  , _data :: [Int]
  } deriving Generic

instance ToJSON JobApiRecord where
  toJSON = genericToJSON dropUnderscore


data PieSliceApiRecord = PieSliceApiRecord {
    _name :: Text
  , _y    :: Integer
  } deriving Generic

instance ToJSON PieSliceApiRecord where
  toJSON = genericToJSON dropUnderscore
