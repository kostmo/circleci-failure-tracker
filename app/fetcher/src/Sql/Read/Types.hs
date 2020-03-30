{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Sql.Read.Types where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.Int                    (Int64)

import qualified AuthStages
import qualified Builds
import qualified CommitBuilds
import qualified DbHelpers
import qualified JsonUtils


maxApiPrCommentRevisionsToFetch :: Int
maxApiPrCommentRevisionsToFetch = 1000


hiddenContextLinecount :: Int
hiddenContextLinecount = 1000


circleCIProviderIndex :: Int64
circleCIProviderIndex = 3


type DbIO a = ReaderT Connection IO a


type AuthDbIO a = ReaderT AuthConnection IO a


-- | For use with ReaderT
data AuthConnection = AuthConnection {
    getConn :: Connection
  , getUser :: AuthStages.Username
  }


runQuery sql = do
  conn <- ask
  liftIO $ query_ conn sql



data TimeRange =
    Bounded (DbHelpers.StartEnd UTCTime)
  | StartOnly UTCTime


queryParmsFromTimeRange :: TimeRange -> (UTCTime, Maybe UTCTime)
queryParmsFromTimeRange time_bounds =
  case time_bounds of
    Bounded (DbHelpers.StartEnd start_time end_time) -> (start_time, Just end_time)
    StartOnly start_time -> (start_time, Nothing)



newtype RecordCount = NewRecordCount Int64
  deriving Show

newtype ElasticBeanstalkWorkerEventID = NewElasticBeanstalkWorkerEventID {getId :: Int64}

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
