{-# LANGUAGE DeriveGeneric #-}

module Breakages2 where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified AuthStages


data BreakageReport = NewBreakageReport {
    sha1        :: Text
  , description :: Text
  , reporter    :: AuthStages.Username
  } deriving Generic

instance ToJSON BreakageReport
instance FromJSON BreakageReport


