{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module ScanPatterns where

import           Data.Aeson
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple           (FromRow)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           GHC.Generics
import           GHC.Int                              (Int64)

import qualified DbHelpers


newtype PatternId = PatternId Int64
  deriving (Show, Generic, FromRow)

instance ToJSON PatternId
instance FromJSON PatternId

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField PatternId where
  fromField f mdata = PatternId <$> fromField f mdata


data MatchExpression =
    RegularExpression
      Text
      Bool -- ^ has nondeterminisitic values (e.g. timestamps; some capture groups are still deterministic)
  | LiteralExpression Text
  deriving (Show, Generic)

instance ToJSON MatchExpression
instance FromJSON MatchExpression


toMatchExpression :: Bool -> Text -> Bool -> MatchExpression
toMatchExpression is_regex expression is_deterministic = if is_regex
  then ScanPatterns.RegularExpression expression is_deterministic
  else ScanPatterns.LiteralExpression expression


is_regex :: MatchExpression -> Bool
is_regex = \case
  RegularExpression _ _ -> True
  LiteralExpression _   -> False


is_nondeterministic :: MatchExpression -> Bool
is_nondeterministic = \case
  RegularExpression _ x -> x
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
