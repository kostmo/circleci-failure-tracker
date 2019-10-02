module Lib where

import GHC.Generics
import Data.Aeson

import Aws.Lambda

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic)
instance FromJSON Person
instance ToJSON Person

handler :: Person -> Context -> IO (Either String Person)
handler person context =
  if age person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")




