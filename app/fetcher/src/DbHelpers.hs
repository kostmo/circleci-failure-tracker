{-# LANGUAGE DeriveGeneric #-}

module DbHelpers where

import           Control.Arrow                      ((&&&))
import           Data.Aeson
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict                as HashMap
import           Data.List                          (intercalate)
import           Data.List.Split                    (splitOn)
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           GHC.Generics
import           GHC.Int                            (Int64)


data OwnerAndRepo = OwnerAndRepo {
    owner :: String
  , repo  :: String
  }


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
  } deriving Generic

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


get_connection :: DbConnectionData -> IO Connection
get_connection conn_data = connect $ defaultConnectInfo {
      connectUser = dbUsername conn_data
    , connectPassword = dbPassword conn_data
    , connectDatabase = dbName conn_data
    , connectHost = dbHostname conn_data
    }
