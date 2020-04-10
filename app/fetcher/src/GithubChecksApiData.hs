{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module GithubChecksApiData where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data AppWrapper = AppWrapper {
    slug :: Text
  , name :: Text
  } deriving (Show, Generic, FromJSON)



