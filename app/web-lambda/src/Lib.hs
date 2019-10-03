module Lib where

import           Data.Aeson
import           GHC.Generics

import           Aws.Lambda

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Generic, Show)
instance FromJSON Person
instance ToJSON Person

handler :: Person -> Context -> IO (Either String Person)
handler person context = do
  putStrLn $ unwords [
      "Invoked handler with:"
    , show person
    ]

  if age person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")




