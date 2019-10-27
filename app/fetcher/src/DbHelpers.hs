{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DbHelpers where

import           Control.Arrow                        ((&&&))
import           Data.Aeson
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.List                            (intercalate)
import           Data.List.Split                      (splitOn)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (field, fromRow)
import           Database.PostgreSQL.Simple.Range     (PGRange (PGRange),
                                                       RangeBound (..))
import           Database.PostgreSQL.Simple.Types     (PGArray, fromPGArray)
import           GHC.Generics
import           GHC.Int                              (Int64)

import qualified JsonUtils


instance (ToJSON a) => ToJSON (PGArray a) where
  toJSON = toJSON . fromPGArray


-- | XXX Note: this is an an arbitrary implementation
-- for my purpose.
instance (ToJSON a) => ToJSON (RangeBound a) where
  toJSON x = case x of
    NegInfinity -> toJSON (Nothing :: Maybe Int64)
    PosInfinity -> toJSON (Nothing :: Maybe Int64)
    Inclusive x -> toJSON $ Just x
    Exclusive x -> toJSON $ Just x


instance (ToJSON a) => ToJSON (PGRange a) where
  toJSON (PGRange x y) = toJSON [x, y]


data BenchmarkedResponse a b = BenchmarkedResponse {
    _timing  :: a
  , _content :: b
  } deriving Generic

instance (ToJSON a, ToJSON b) => ToJSON (BenchmarkedResponse a b) where
  toJSON = genericToJSON JsonUtils.dropUnderscore


-- | TODO Use this for more weekly data
data TimestampedDatum a = TimestampedDatum {
    _timestamp :: UTCTime
  , _record    :: a
  } deriving Generic

instance (ToJSON a) => ToJSON (TimestampedDatum a) where
  toJSON = genericToJSON JsonUtils.dropUnderscore

instance (FromRow a) => FromRow (TimestampedDatum a) where
  fromRow = TimestampedDatum <$> field <*> fromRow


data OwnerAndRepo = OwnerAndRepo {
    owner :: String
  , repo  :: String
  }


-- | This should only be used for deserializing HTML form values
-- into lists. For converting database arrays to Haskell records,
-- use the PGArray datatype.
--
-- Compare to "splitAggText"
cleanSemicolonDelimitedList :: String -> [Text]
cleanSemicolonDelimitedList =
  filter (not . T.null) . map (T.strip . T.pack) . splitOn ";"


-- | Returned by database queries as semicolon-delimited string.
-- Parsed into individual values in the FromRow function.
newtype SemicolonDelimitedDbText = SemicolonDelimitedDbText [Text]
  deriving Generic

instance FromField SemicolonDelimitedDbText where
  fromField f mdata = SemicolonDelimitedDbText . DbHelpers.cleanSemicolonDelimitedList <$> fromField f mdata

instance ToJSON SemicolonDelimitedDbText


-- | Compare to "cleanSemicolonDelimitedList"
--
-- TODO Eliminate this function by using array_agg
-- in the database query instead of string_agg
splitAggText :: String -> [String]
splitAggText = filter (not . null) . splitOn ";"


githubRepoApiPrefix :: DbHelpers.OwnerAndRepo -> String
githubRepoApiPrefix (DbHelpers.OwnerAndRepo repo_owner repo_name) = intercalate "/" [
    "https://api.github.com/repos"
  , repo_owner
  , repo_name
  ]


data WithAuthorship a = WithAuthorship {
    author  :: Text
  , created :: UTCTime
  , payload :: a
  } deriving Generic

instance ToJSON a => ToJSON (WithAuthorship a)
instance FromJSON a => FromJSON (WithAuthorship a)


data WithTypedId a b = WithTypedId {
    typed_id     :: a
  , typed_record :: b
  } deriving Generic

instance (ToJSON a, ToJSON b) => ToJSON (WithTypedId a b)


data WithId a = WithId {
    db_id  :: Int64
  , record :: a
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (WithId a)
instance FromJSON a => FromJSON (WithId a)

instance FromRow a => FromRow (WithId a) where
  fromRow = WithId <$> field <*> fromRow


data DbConnectionData = NewDbConnectionData {
    dbHostname :: String
  , dbName     :: String
  , dbUsername :: String
  , dbPassword :: String
  }


as_tuple :: WithId a -> (Int64, a)
as_tuple = db_id &&& record


to_dict :: [WithId a] -> HashMap Int64 a
to_dict = HashMap.fromList . map as_tuple


setSessionStatementTimeout ::
     Connection
  -> Integer -- ^ seconds
  -> IO ()
setSessionStatementTimeout conn seconds = do
  execute conn sql $ Only milliseconds
  return ()
  where
    milliseconds = seconds * 1000
    sql = "SET SESSION statement_timeout = ?;"


getConnectionWithStatementTimeout ::
     DbConnectionData
  -> Integer
  -> IO Connection
getConnectionWithStatementTimeout conn_data timeout_seconds = do
  conn <- get_connection conn_data
  setSessionStatementTimeout conn timeout_seconds
  return conn


get_connection :: DbConnectionData -> IO Connection
get_connection conn_data = connect $ defaultConnectInfo {
      connectUser = dbUsername conn_data
    , connectPassword = dbPassword conn_data
    , connectDatabase = dbName conn_data
    , connectHost = dbHostname conn_data
    }
