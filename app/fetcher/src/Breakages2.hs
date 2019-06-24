{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Breakages2 where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           GHC.Int      (Int64)

import qualified AuthStages


data BreakageReport = NewBreakageReport {
    sha1        :: Text
  , description :: Text
  , reporter    :: AuthStages.Username
  } deriving Generic

instance ToJSON BreakageReport
instance FromJSON BreakageReport


data ResolutionReport = NewResolutionReport {
    sha1     :: Text
  , cause    :: Int64
  , reporter :: AuthStages.Username
  } deriving Generic

instance ToJSON ResolutionReport
instance FromJSON ResolutionReport


