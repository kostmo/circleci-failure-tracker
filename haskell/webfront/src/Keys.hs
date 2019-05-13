{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Keys where

import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

import qualified           AuthConfig


-- | http://developer.github.com/v3/oauth/
githubKey :: AuthConfig.GithubConfig -> OAuth2
githubKey auth_config = OAuth2 { oauthClientId = "Iv1.27ef9a05a9f094c7"
                    , oauthClientSecret = AuthConfig.client_secret auth_config
                    , oauthCallback = Just [uri|http://localhost:3000/oauth2/callback|]
                    , oauthOAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
                    , oauthAccessTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
                    }
  where
    callback_url = if AuthConfig.is_local auth_config
      then "http://localhost:3000/oauth2/callback"
      else "https://circle.pytorch.org/oauth2/callback"


