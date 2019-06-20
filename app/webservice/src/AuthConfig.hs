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
  }


initIdps :: CacheStore -> GithubConfig -> IO ()
initIdps c auth_config = insertIDPData c $ mkIDPData auth_config


mkIDPData :: GithubConfig -> IDPData
mkIDPData auth_config = IDPData (Utils.createCodeUri (githubKey auth_config) [("state", "Github.test-state-123")]) Nothing (idpLabel Github.Github)


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
      else [uri|https://circle.pytorch.org/api/github-auth-callback|]



getLoginUrl :: GithubConfig -> Text
getLoginUrl auth_config =
  TL.toStrict $ codeFlowUri idp_data
  where
    idp_data = mkIDPData auth_config
