{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthStages where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import qualified JsonUtils

newtype Username = Username Text deriving (Generic, Show)

instance ToJSON Username
instance FromJSON Username


data AuthenticationFailureStage =
    FailMembershipDetermination Text
  | FailUsernameDetermination
  | FailLoginRequired
  | FailOrgMembership Username Text
  deriving Show


getMessage :: AuthenticationFailureStage -> Text
getMessage (FailMembershipDetermination msg) = "Could not determine org membership: " <> msg
getMessage FailUsernameDetermination = "Could not determine username."
getMessage FailLoginRequired = "token lookup failed; you need to log in!"
getMessage (FailOrgMembership (Username username_text) x) = "User \"" <> username_text <> "\" is not a member of the organization \"" <> x <> "\"!"


instance JsonUtils.WithErrorMessage AuthenticationFailureStage where
  getMessage = getMessage


instance ToJSON AuthenticationFailureStage where
  toJSON = toJSON . getMessage

