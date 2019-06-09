{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MatchOccurrences where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           GHC.Int      (Int64)

import qualified JsonUtils
import qualified ScanPatterns


newtype MatchId = MatchId Int64
  deriving (Show, Generic)

instance ToJSON MatchId
instance FromJSON MatchId


data MatchOccurrencesForBuild = MatchOccurrencesForBuild {
    _build_step  :: Text
  , _pattern_id  :: ScanPatterns.PatternId
  , _match_id    :: MatchId
  , _line_number :: Int
  , _line_count  :: Int
  , _line_text   :: Text
  , _span_start  :: Int
  , _span_end    :: Int
  , _specificity :: Int
  } deriving Generic

instance ToJSON MatchOccurrencesForBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore
