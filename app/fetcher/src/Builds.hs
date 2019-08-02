{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Builds where

import           Data.Aeson
import           Data.Text                            (Text)
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple           (FromRow)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (field, fromRow)
import           GHC.Generics
import           GHC.Int                              (Int64)

import qualified DbHelpers

masterName :: Text
masterName = "master"


newtype RawCommit = RawCommit Text
 deriving (Generic, Show, FromRow)

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField RawCommit where
  fromField f mdata = RawCommit <$> fromField f mdata


instance ToJSON RawCommit
instance FromJSON RawCommit

newtype BuildNumber = NewBuildNumber Int64
  deriving (Show, Generic, Eq, Ord, FromRow)

instance ToJSON BuildNumber
instance FromJSON BuildNumber

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField BuildNumber where
  fromField f mdata = NewBuildNumber <$> fromField f mdata


newtype UniversalBuildId = UniversalBuildId Int64
  deriving (Show, Generic, Eq, Ord)

instance ToJSON UniversalBuildId

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField UniversalBuildId where
  fromField f mdata = UniversalBuildId <$> fromField f mdata


newtype BuildStepId = NewBuildStepId Int64
  deriving (Show, Generic)

instance ToJSON BuildStepId
instance FromJSON BuildStepId


data CiProvider = CiProvider {
    icon_url :: Text
  , label    :: Text
  } deriving Generic

instance ToJSON CiProvider


data UniversalBuild = UniversalBuild {
    provider_buildnum :: BuildNumber
  , provider_id       :: Int64
  , build_namespace   :: Text
  , succeeded         :: Bool
  , sha1              :: RawCommit
  } deriving Generic

instance ToJSON UniversalBuild

instance FromRow UniversalBuild where
  fromRow = do
    provider_buildnum <- field
    provider_id <- field
    build_namespace <- field
    succeeded <- field
    sha1 <- field

    let wrapped_commit = Builds.RawCommit sha1
        u_build_obj = UniversalBuild
          provider_buildnum
          provider_id
          build_namespace
          succeeded
          wrapped_commit

    return u_build_obj


data StorableBuild = StorableBuild {
    universal_build :: DbHelpers.WithId UniversalBuild -- ^ this already came from database
  , build_record    :: Build
  } deriving Generic

instance ToJSON StorableBuild


instance FromRow StorableBuild where
  fromRow = do
    global_build_num <- field

    u_build_obj <- fromRow

    let wrapped_commit = sha1 u_build_obj
        provider_build_number = provider_buildnum u_build_obj

    queued_at <- field
    job_name <- field
    branch <- field
    start_time <- field
    stop_time <- field

    let inner_build_obj = NewBuild
          provider_build_number
          wrapped_commit
          queued_at
          job_name
          branch
          start_time
          stop_time

    return $ StorableBuild
      (DbHelpers.WithId global_build_num u_build_obj)
      inner_build_obj


data Build = NewBuild {
    build_id     :: BuildNumber -- ^ deprecated; should come from UniversalBuild
  , vcs_revision :: RawCommit
  , queued_at    :: UTCTime
  , job_name     :: Text
  , branch       :: Maybe Text
  , start_time   :: Maybe UTCTime
  , stop_time    :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON Build
instance FromJSON Build


data BuildWithStepFailure = BuildWithStepFailure {
    build_object        :: Build
  , step_failure_object :: BuildStepFailure
  } deriving Show


data BuildStepFailure = NewBuildStepFailure {
    step_name    :: Text
  , failure_mode :: BuildFailureMode
  } deriving Show


-- | There can be different modes in which the build fails.
data BuildFailureMode =
    BuildTimeoutFailure
  | ScannableFailure BuildFailureOutput
  deriving Show


newtype BuildFailureOutput = NewBuildFailureOutput {
    log_url :: Text
  } deriving Show
