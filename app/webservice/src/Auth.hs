{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Auth (
    getAuthenticatedUser
  , logoutH
  , callbackH
  , githubAuthTokenSessionKey
  ) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Bifunctor
import qualified Data.ByteString.Char8      as BSU
import qualified Data.ByteString.Lazy       as LBS
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Vault.Lazy            as Vault
import           Network.HTTP.Conduit       hiding (Request)
import qualified Network.OAuth.OAuth2       as OAuth2
import           Network.Wai                (Request, vault)
import           Network.Wai.Session        (Session)
import           Prelude
import           URI.ByteString             (parseURI, strictURIParserOptions)
import           Web.Scotty
import           Web.Scotty.Internal.Types

import qualified AuthConfig
import qualified AuthStages
import qualified Constants
import qualified Github
import qualified GithubApiFetch
import           Session
import           SillyMonoids               ()
import           Types
import           Utils


githubAuthTokenSessionKey :: String
githubAuthTokenSessionKey = "github_api_token"


wrap_login_err ::
     T.Text
  -> AuthStages.AuthenticationFailureStageInfo
  -> AuthStages.BackendFailure a
wrap_login_err login_url = AuthStages.AuthFailure . AuthStages.AuthenticationFailure (Just $ AuthStages.LoginUrl login_url)


getAuthenticatedUserByToken ::
     OAuth2.AccessToken -- ^ token
  -> AuthConfig.GithubConfig
  -> (AuthStages.Username -> IO (Either a b))
  -> IO (Either (AuthStages.BackendFailure a) b)
getAuthenticatedUserByToken wrapped_token github_config callback = do

  mgr <- newManager tlsManagerSettings
  let api_support_data = GithubApiFetch.GitHubApiSupport mgr wrapped_token

  runExceptT $ do
    Types.LoginUser _login_name login_alias <- ExceptT $
      first (const $ wrap_login_err login_url AuthStages.FailUsernameDetermination) <$> GithubApiFetch.fetchUser api_support_data

    let username_text = TL.toStrict login_alias
    is_org_member <- ExceptT $ do
      either_membership <- isOrgMember (AuthConfig.personal_access_token github_config) username_text
      return $ first (wrap_login_err login_url) either_membership

    unless is_org_member $ except $
      Left $ AuthStages.AuthFailure $ AuthStages.AuthenticationFailure (Just $ AuthStages.LoginUrl login_url)
        $ AuthStages.FailOrgMembership (AuthStages.Username username_text) $ T.pack Constants.project_name

    ExceptT $ fmap (first AuthStages.DbFailure) $
      callback $ AuthStages.Username username_text

  where
    login_url = AuthConfig.getLoginUrl github_config


getAuthenticatedUser ::
     Request
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> (AuthStages.Username -> IO (Either a b))
  -> IO (Either (AuthStages.BackendFailure a) b)
getAuthenticatedUser rq session github_config callback = do

  u <- sessionLookup githubAuthTokenSessionKey
  case u of
    Nothing -> return $ Left $ wrap_login_err login_url AuthStages.FailLoginRequired
    Just api_token -> getAuthenticatedUserByToken (OAuth2.AccessToken $ T.pack api_token) github_config callback

  where
    Just (sessionLookup, _sessionInsert) = Vault.lookup session $ vault rq
    login_url = AuthConfig.getLoginUrl github_config


redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"


errorM :: TL.Text -> ActionM ()
errorM = throwError . ActionError


logoutH :: CacheStore -> ActionM ()
logoutH c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  let idp = Github.Github
  liftIO (removeKey c (idpLabel idp)) >> redirectToHomeM


callbackH :: CacheStore -> AuthConfig.GithubConfig -> (String -> IO ()) -> ActionT TL.Text IO ()
callbackH c github_config session_insert = do
  pas <- params
  let codeP = paramValue "code" pas
      stateP = paramValue "state" pas
  when (null codeP) $ errorM "callbackH: no code from callback request"
  when (null stateP) $ errorM "callbackH: no state from callback request"

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
        Left err    -> errorM $ "fetchTokenAndUser: " `TL.append` err

  where lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)
        updateIdp c1 oldIdpData luser = liftIO $ insertIDPData c1 (oldIdpData {loginUser = Just luser })
        idp = Github.Github


tryFetchUser ::
     AuthConfig.GithubConfig
  -> TL.Text           -- ^ code
  -> (String -> IO ())
  -> IO (Either TL.Text LoginUser)
tryFetchUser github_config code session_insert = do
  mgr <- newManager tlsManagerSettings
  token <- OAuth2.fetchAccessToken mgr (AuthConfig.githubKey github_config) (OAuth2.ExchangeToken $ TL.toStrict code)

  case token of
    Right at -> do
      let access_token_object = OAuth2.accessToken at
          access_token_string = T.unpack $ OAuth2.atoken access_token_object

      liftIO $ session_insert access_token_string
      GithubApiFetch.fetchUser $ GithubApiFetch.GitHubApiSupport mgr access_token_object

    Left e   -> return $ Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e


-- | The Github API for this returns an empty response, using
-- status codes 204 or 404 to represent success or failure, respectively.
isOrgMemberInner :: OAuth2.OAuth2Result TL.Text LBS.ByteString -> Bool
isOrgMemberInner either_response = case either_response of
  Left (OAuth2.OAuth2Error _either_parsed_err _maybe_description _maybe_uri) -> False
  Right _ -> True


-- | Alternate (user-centric) API endpoint is:
-- https://developer.github.com/v3/orgs/members/#get-your-organization-membership
isOrgMember :: OAuth2.AccessToken -> T.Text -> IO (Either AuthStages.AuthenticationFailureStageInfo Bool)
isOrgMember wrapped_token username = do
  mgr <- newManager tlsManagerSettings

  -- Note: This query is currently using a Personal Access Token from a pytorch org member.
  -- TODO This must be converted to an App token.
  let api_support_data = GithubApiFetch.GitHubApiSupport mgr wrapped_token

  case either_membership_query_uri of
    Left _x -> return $ Left $ AuthStages.FailMembershipDetermination $ "Bad URL: " <> url_string
    Right membership_query_uri -> do
      either_response <- OAuth2.authGetBS mgr wrapped_token membership_query_uri
      return $ Right $ isOrgMemberInner either_response

  where
    url_string = "https://api.github.com/orgs/" <> T.pack Constants.project_name <> "/members/" <> username
    either_membership_query_uri = parseURI strictURIParserOptions $ BSU.pack $ T.unpack url_string
