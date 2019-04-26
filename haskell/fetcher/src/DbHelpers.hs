module DbHelpers where

import           Control.Arrow              ((&&&))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Database.PostgreSQL.Simple
import           GHC.Int                    (Int64)

data WithId a = WithId {
    db_id  :: Int64
  , record :: a
  }


as_tuple :: WithId a -> (Int64, a)
as_tuple = db_id &&& record


to_dict :: [WithId a] -> HashMap Int64 a
to_dict = HashMap.fromList . map as_tuple


get_connection :: IO Connection
get_connection = connect $ defaultConnectInfo {
      connectUser = "logan"
    , connectPassword = "logan01"
    , connectDatabase = "loganci"
    }
