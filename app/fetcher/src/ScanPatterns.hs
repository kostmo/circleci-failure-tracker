{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module ScanPatterns where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified DbHelpers


data MatchExpression =
    RegularExpression
      Text
      Bool -- ^ has nondeterminisitic values (e.g. timestamps; some capture groups are still deterministic)
  | LiteralExpression Text
  deriving (Show, Generic)

instance ToJSON MatchExpression
instance FromJSON MatchExpression


is_regex :: MatchExpression -> Bool
is_regex = \case
  RegularExpression _ _ -> True
  LiteralExpression _   -> False


pattern_text :: MatchExpression -> Text
pattern_text = \case
  RegularExpression x _ -> x
  LiteralExpression x -> x


data Pattern = NewPattern {
    expression       :: MatchExpression
  , description      :: Text
  , tags             :: [Text]
  , applicable_steps :: [Text]
  , specificity      :: Int
  , is_retired       :: Bool
  , lines_from_end   :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON Pattern
instance FromJSON Pattern


type DbPattern = DbHelpers.WithId Pattern


data MatchSpan = NewMatchSpan {
    start :: Int
  , end   :: Int
  } deriving Generic

instance ToJSON MatchSpan
instance FromJSON MatchSpan


data MatchDetails = NewMatchDetails {
    line_text   :: Text
  , line_number :: Int
  , span        :: MatchSpan
  } deriving Generic

instance ToJSON MatchDetails


data ScanMatch = NewScanMatch {
    scanned_pattern :: DbPattern
  , match_details   :: MatchDetails
  } deriving Generic

instance ToJSON ScanMatch
