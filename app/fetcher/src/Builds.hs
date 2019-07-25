{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Builds where

import           Data.Aeson
import           Data.Text                            (Text)
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple           (FromRow)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           GHC.Generics
import           GHC.Int                              (Int64)

newtype RawCommit = RawCommit Text
 deriving (Generic, Show)

instance ToJSON RawCommit
instance FromJSON RawCommit

newtype BuildNumber = NewBuildNumber Int64
  deriving (Show, Generic, Eq, Ord, FromRow)

instance ToJSON BuildNumber
instance FromJSON BuildNumber

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField BuildNumber where
  fromField f mdata = NewBuildNumber <$> fromField f mdata


newtype BuildStepId = NewBuildStepId Int64
  deriving (Show, Generic)

instance ToJSON BuildStepId
instance FromJSON BuildStepId



data UniversalBuild = UniversalBuild {
    provider_buildnum :: BuildNumber
  , provider_id       :: Int64
  , build_namespace   :: Text
  } deriving (Show, Generic)


data Build = NewBuild {
    build_id     :: BuildNumber
  , vcs_revision :: RawCommit
  , queued_at    :: UTCTime
  , job_name     :: Text
  , branch       :: Text
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
