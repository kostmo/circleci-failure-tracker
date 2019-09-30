{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module LambdaRoutes where

import           Aws.Lambda
import           Data.Aeson
import           GHC.Generics


import qualified Data.ByteString.Lazy.Char8 as ByteString

-- Input
data Event = Event
  { resource :: String
  , body     :: String
  } deriving (Generic, FromJSON)

-- Output
data Response = Response
  { statusCode:: Int
  , body       :: String
  } deriving (Generic, ToJSON)

-- Type that we decode from the 'body' of 'Event'
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Generic, FromJSON, ToJSON)

greet :: Person -> String
greet person =
  "Hello, " ++ name person ++ "!"

handler :: Event -> Context -> IO (Either String Response)
handler Event{..} context = do
  case decode (ByteString.pack body) of
    Just person ->
      pure $ Right Response
        { statusCode = 200
        , body = greet person
        }
    Nothing ->
      pure $ Right Response
        { statusCode = 200
        , body = "bad person"
        }


