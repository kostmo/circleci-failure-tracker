{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Auth where

import           Control.Monad
import           Control.Monad.Error.Class
import qualified Data.ByteString.Lazy as LBS
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Either as Either
import           Data.Bifunctor
import           URI.ByteString (parseURI, strictURIParserOptions)
import           Data.Maybe
import qualified Data.Text.Lazy                as TL
import qualified Data.Text                as T
import           Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BSU
import           Network.HTTP.Types
import           Network.OAuth.OAuth2 as OAuth2
import           Prelude
import           Web.Scotty
import           Web.Scotty.Internal.Types


import qualified AuthConfig
import qualified IDP.Github          as IGithub
import           Session
import           Types
import           Utils
import           Views

import qualified IDP.Github                    as Github
import qualified Keys


targetOrganization = "pytorch"

debug = True

--------------------------------------------------
-- * Handlers
--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

errorM :: TL.Text -> ActionM ()
errorM = throwError . ActionError


globalErrorHandler :: TL.Text -> ActionM ()
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


--callbackH :: CacheStore -> AuthConfig.GithubConfig -> ( ->) -> ActionM ()
callbackH c github_config session_insert = do
  pas <- params
  let codeP = paramValue "code" pas
      stateP = paramValue "state" pas
  when (null codeP) (errorM "callbackH: no code from callback request")
  when (null stateP) (errorM "callbackH: no state from callback request")

  fetchTokenAndUser c github_config (head codeP) session_insert


fetchTokenAndUser :: CacheStore
                  -> AuthConfig.GithubConfig
                  -> TL.Text           -- ^ code
                  -> (String -> IO ())
                  -> ActionM ()
fetchTokenAndUser c github_config code session_insert = do
  maybeIdpData <- lookIdp c idp

  case maybeIdpData of
    Nothing -> errorM "fetchTokenAndUser: cannot find idp data from cache"
    Just idpData -> do

      result <- liftIO $ tryFetchUser github_config code session_insert

      case result of
        Right luser -> updateIdp c idpData luser >> redirectToHomeM
        Left err    -> errorM ("fetchTokenAndUser: " `TL.append` err)

  where lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)
        updateIdp c1 oldIdpData luser = liftIO $ insertIDPData c1 (oldIdpData {loginUser = Just luser })
        idp = IGithub.Github


data GitHubApiSupport = GitHubApiSupport {
    tls_manager :: Manager
  , access_token :: AccessToken
  }


-- TODO: may use Exception monad to capture error in this IO monad
--
tryFetchUser ::
     AuthConfig.GithubConfig
  -> TL.Text           -- ^ code
  -> (String -> IO ())
  -> IO (Either TL.Text LoginUser)
tryFetchUser github_config code session_insert = do
  mgr <- newManager tlsManagerSettings
  token <- fetchAccessToken mgr (Keys.githubKey github_config) (ExchangeToken $ TL.toStrict code)
  when debug (print token)
  case token of
    Right at -> do
      let access_token_object = accessToken at
          access_token_string = T.unpack $ OAuth2.atoken access_token_object
      liftIO $ session_insert access_token_string

      fetchUser (GitHubApiSupport mgr access_token_object)

    Left e   -> return (Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e)


-- * Fetch UserInfo
--
fetchUser :: GitHubApiSupport -> IO (Either TL.Text LoginUser)
fetchUser (GitHubApiSupport mgr token) = do
  re <- do
    r <- authGetJSON mgr token Github.userInfoUri
    return (second IGithub.toLoginUser r)

  return (first displayOAuth2Error re)


displayOAuth2Error :: OAuth2Error Errors -> TL.Text
displayOAuth2Error = TL.pack . show


-- | The Github API for this returns an empty response, using
-- status codes 204 or 404 to represent success or failure, respectively.
isOrgMemberInner :: OAuth2.OAuth2Result TL.Text LBS.ByteString -> Bool
isOrgMemberInner either_response = case either_response of
  Left (OAuth2.OAuth2Error _either_parsed_err _maybe_description _maybe_uri) -> False
  Right _ -> True


-- | Alternate (user-centric) API endpoint is: https://developer.github.com/v3/orgs/members/#get-your-organization-membership
isOrgMember :: T.Text -> T.Text -> IO (Either T.Text Bool)
isOrgMember personal_access_token username = do
  mgr <- newManager tlsManagerSettings
  -- Note: This query is currently using a Personal Access Token from a pytorch org member
  -- This must be converted to an App token.
  let api_support_data = Auth.GitHubApiSupport mgr wrapped_token

  case either_membership_query_uri of
    Left x -> return $ Left ("Bad URL: " <> url_string)
    Right membership_query_uri -> do
      either_response <- authGetBS mgr wrapped_token membership_query_uri
      return $ Right $ isOrgMemberInner either_response
  where
    wrapped_token = OAuth2.AccessToken personal_access_token
    url_string = "https://api.github.com/orgs/" <> targetOrganization <> "/members/" <> username
    either_membership_query_uri = parseURI strictURIParserOptions $ BSU.pack $ T.unpack url_string


