{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AuthConfig where

import           Data.Text            (Text)
import qualified Data.Text.Lazy       as TL
import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

import qualified Github
import           Session
import           Types
import qualified Utils


data GithubConfig = NewGithubConfig {
    is_local              :: Bool
  , client_id             :: Text
  , client_secret         :: Text
  , personal_access_token :: AccessToken
  , webhook_secret        :: Text
  , admin_password        :: Text
  , no_force_ssl          :: Bool
  }


initIdps :: CacheStore -> GithubConfig -> IO ()
initIdps c = insertIDPData c . mkIDPData


mkIDPData :: GithubConfig -> IDPData
mkIDPData auth_config = IDPData code_uri Nothing $ idpLabel Github.Github
  where
    code_uri = Utils.createCodeUri (githubKey auth_config) [("state", "Github.test-state-123")]


-- | http://developer.github.com/v3/oauth/
githubKey :: GithubConfig -> OAuth2
githubKey auth_config = OAuth2 {
      oauthClientId = client_id auth_config
    , oauthClientSecret = client_secret auth_config
    , oauthCallback = Just callback_url
    , oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
    , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    }
  where
    callback_url = if is_local auth_config
      then [uri|http://localhost:3001/api/github-auth-callback|]
      else [uri|https://dr.pytorch.org/api/github-auth-callback|]


getLoginUrl :: GithubConfig -> Text
getLoginUrl = TL.toStrict . codeFlowUri . mkIDPData
