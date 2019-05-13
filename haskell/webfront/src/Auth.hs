{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Auth where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as TL
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.OAuth.OAuth2
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Static
import           Prelude
import           Web.Scotty
import           Web.Scotty.Internal.Types


import qualified AuthConfig
import qualified IDP.Github          as IGithub
import           IDP
import           Session
import           Types
import           Utils
import           Views

import qualified IDP.Github                    as Github
import qualified Keys


debug = True

--------------------------------------------------
-- * Handlers
--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

logoutH :: CacheStore -> ActionM ()
logoutH c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  let idp = IGithub.Github
  liftIO (removeKey c (idpLabel idp)) >> redirectToHomeM


indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= overviewTpl

callbackH :: CacheStore -> AuthConfig.GithubConfig -> ActionM ()
callbackH c github_config = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "callbackH: no code from callback request")
  when (null stateP) (errorM "callbackH: no state from callback request")

  let idp = IGithub.Github
  fetchTokenAndUser c github_config (head codeP) idp


fetchTokenAndUser :: (HasLabel a)
                  => CacheStore
                  -> AuthConfig.GithubConfig
                  -> TL.Text           -- ^ code
                  -> a
                  -> ActionM ()
fetchTokenAndUser c github_config code idp = do
  maybeIdpData <- lookIdp c idp
  when (isNothing maybeIdpData) (errorM "fetchTokenAndUser: cannot find idp data from cache")

  let idpData = fromJust maybeIdpData
  result <- liftIO $ tryFetchUser idp github_config code

  case result of
    Right luser -> updateIdp c idpData luser >> redirectToHomeM
    Left err    -> errorM ("fetchTokenAndUser: " `TL.append` err)

  where lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)
        updateIdp c1 oldIdpData luser = liftIO $ insertIDPData c1 (oldIdpData {loginUser = Just luser })

-- TODO: may use Exception monad to capture error in this IO monad
--
tryFetchUser :: (HasLabel a)
  => a
  -> AuthConfig.GithubConfig
  -> TL.Text           -- ^ code
  -> IO (Either Text LoginUser)
tryFetchUser idp github_config code = do
  mgr <- newManager tlsManagerSettings
  token <- fetchAccessToken mgr (Keys.githubKey github_config) (ExchangeToken $ TL.toStrict code)
  when debug (print token)
  case token of
    Right at -> fetchUser idp mgr (accessToken at)
    Left e   -> return (Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e)

-- * Fetch UserInfo
--
fetchUser :: a -> Manager -> AccessToken -> IO (Either Text LoginUser)
fetchUser idp mgr token = do
  re <- do
    r <- authGetJSON mgr token Github.userInfoUri
    return (second IGithub.toLoginUser r)

  return (first displayOAuth2Error re)

displayOAuth2Error :: OAuth2Error Errors -> Text
displayOAuth2Error = TL.pack . show

