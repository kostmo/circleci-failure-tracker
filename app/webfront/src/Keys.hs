{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Keys where

import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

import qualified           AuthConfig


-- | http://developer.github.com/v3/oauth/
githubKey :: AuthConfig.GithubConfig -> OAuth2
githubKey auth_config = OAuth2 {
      oauthClientId = AuthConfig.client_id auth_config
    , oauthClientSecret = AuthConfig.client_secret auth_config
    , oauthCallback = Just callback_url
    , oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
    , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    }
  where
    callback_url = if AuthConfig.is_local auth_config
      then [uri|http://localhost:3000/api/github-auth-callback|]
      else [uri|https://circle.pytorch.org/api/github-auth-callback|]


