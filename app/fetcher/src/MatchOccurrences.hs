{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MatchOccurrences where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified JsonUtils


data MatchOccurrencesForBuild = MatchOccurrencesForBuild {
    _build_step  :: Text
  , _pattern_id  :: Int
  , _line_number :: Int
  , _line_count  :: Int
  , _line_text   :: Text
  , _span_start  :: Int
  , _span_end    :: Int
  , _specificity :: Int
  } deriving Generic

instance ToJSON MatchOccurrencesForBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore
