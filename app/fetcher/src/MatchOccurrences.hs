{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MatchOccurrences where

import           Data.Aeson
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Database.PostgreSQL.Simple           (FromRow)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           GHC.Generics
import           GHC.Int                              (Int64)

import qualified JsonUtils
import qualified ScanPatterns


newtype MatchId = MatchId Int64
  deriving (Show, Generic)

instance ToJSON MatchId
instance FromJSON MatchId

-- TODO do error handling: http://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple-FromField.html
instance FromField MatchId where
  fromField f mdata = MatchId <$> fromField f mdata


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
  } deriving (Generic, FromRow)

instance ToJSON MatchOccurrencesForBuild where
  toJSON = genericToJSON JsonUtils.dropUnderscore


getMatchedText :: MatchOccurrencesForBuild -> Text
getMatchedText match_obj =
  T.take span_length $ T.drop (_span_start match_obj) $ _line_text match_obj
  where
    span_length = _span_end match_obj - _span_start match_obj
