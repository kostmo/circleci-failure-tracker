{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Sql.ReadTypes where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple (FromRow)
import           GHC.Generics

import qualified Builds
import qualified CommitBuilds
import qualified JsonUtils


type StandardCommitBuildWrapper = CommitBuilds.CommitBuildWrapper CommitBuildSupplementalPayload


data CommitBuildSupplementalPayload = CommitBuildSupplementalPayload {
    is_empirically_determined_flaky :: Bool
  , has_completed_rerun             :: Bool
  , has_triggered_rebuild           :: Bool
  , failure_count                   :: Int
  } deriving (Generic, FromRow, ToJSON)



data UpstreamBrokenJob = UpstreamBrokenJob {
    _job_name            :: Text
  , _breakage_start_time :: UTCTime
  , _breakage_end_time   :: Maybe UTCTime
  , _breakage_start_sha1 :: Builds.RawCommit
  , _breakage_end_sha1   :: Maybe Builds.RawCommit
  , _universal_build     :: Builds.UniversalBuildId
  , _provider            :: Int
  , _provider_build_num  :: Builds.BuildNumber
  , _span_length         :: Maybe Int
  } deriving (Generic, FromRow)

instance ToJSON UpstreamBrokenJob where
  toJSON = genericToJSON JsonUtils.dropUnderscore


extractJobName :: UpstreamBrokenJob -> Text
extractJobName (UpstreamBrokenJob x _ _ _ _ _ _ _ _) = x
