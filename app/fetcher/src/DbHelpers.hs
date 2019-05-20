{-# LANGUAGE DeriveGeneric #-}

module DbHelpers where

import           Control.Arrow              ((&&&))
import           Data.Aeson
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           GHC.Int                    (Int64)


data WithAuthorship a = WithAuthorship {
    author  :: Text
  , created :: UTCTime
  , payload :: a
  } deriving Generic

instance ToJSON a => ToJSON (WithAuthorship a)


data WithId a = WithId {
    db_id  :: Int64
  , record :: a
  } deriving Generic

instance ToJSON a => ToJSON (WithId a)



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
