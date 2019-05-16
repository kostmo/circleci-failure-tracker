{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module WebApi where

import           Data.Aeson
import           Data.Text             (Text)
import           GHC.Generics
import           System.Directory      (doesDirectoryExist)
import qualified System.DiskSpace      as DiskSpace
import           System.FilePath.Posix (takeDirectory)
import           GHC.Int                           (Int64)
import           System.Process        (readProcess)

import qualified Builds
import qualified Constants


data InsertionFailureMode = InsertionFailDatabase | InsertionFailAuthentication


data InsertionResult =
    SuccessResult Int64
  | FailResult InsertionFailureMode String


toInsertionResponse :: InsertionResult -> InsertionResponse
toInsertionResponse result = case result of
  SuccessResult record_id -> InsertionResponse True False False (Just record_id) "Succeeded." Nothing
  FailResult failure_mode error_message -> case failure_mode of
    InsertionFailAuthentication -> InsertionResponse False True False Nothing error_message (Just "/login")
    InsertionFailDatabase -> InsertionResponse False False True Nothing error_message Nothing


-- | TODO This doesn't have to be flat; use sum types in JSON conversion
data InsertionResponse = InsertionResponse {
    _success :: Bool
  , _authentication_failed :: Bool
  , _database_failed :: Bool
  , _pattern_id :: Maybe Int64
  , _message :: String
  , _login_url :: Maybe String
  } deriving Generic

instance ToJSON InsertionResponse where
  toJSON = genericToJSON dropUnderscore


data ApiResponse a = ApiResponse {
    rows :: [a]
  } deriving Generic

instance (ToJSON a) => ToJSON (ApiResponse a)


dropUnderscore = defaultOptions {fieldLabelModifier = drop 1}


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


api_disk_space = do

  cache_dir <- Constants.get_url_cache_basedir
  dir_exists <- doesDirectoryExist cache_dir

  cache_bytes <- if dir_exists
    then do
      output <- readProcess "/usr/bin/du" ["--bytes", cache_dir] ""
      return $ read $ takeWhile (\x -> x /= '\t') output
    else return 0

  let avail_space_reference_dir = if dir_exists
        then cache_dir
        else takeDirectory cache_dir

  avail_bytes <- DiskSpace.getAvailSpace avail_space_reference_dir

  return $ [
      PieSliceApiRecord "Available" avail_bytes
    , PieSliceApiRecord "Consumed" cache_bytes
    ]
