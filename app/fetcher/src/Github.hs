{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Github where
import           Data.Aeson
import           Data.Hashable
import           Data.Text.Lazy    (Text)
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ


data Github = Github deriving (Show, Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github


data GithubUser = GithubUser { name  :: Text
                             , id    :: Integer
                             , login :: Text
                             } deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]


toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser {
    loginUserName = name guser
  , loginAlias = login guser
  }
