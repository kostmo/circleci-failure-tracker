{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Auth where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class    (liftIO)
import           Data.Bifunctor
import qualified Data.ByteString.Char8     as BSU
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (intercalate)
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Vault.Lazy           as Vault
import           Network.HTTP.Conduit      hiding (Request)
import           Network.HTTP.Types
import qualified Network.OAuth.OAuth2      as OAuth2
import           Network.Wai               (Request, vault)
import           Network.Wai.Session       (Session)
import           Prelude
import           URI.ByteString            (parseURI, strictURIParserOptions)
import           Web.Scotty
import           Web.Scotty.Internal.Types

import qualified ApiPost
import qualified AuthConfig
import qualified AuthStages
import qualified DbHelpers
import qualified Github
import qualified Keys
import           Session
import qualified StatusEvent
import           Types
import           Utils
import qualified Webhooks


targetOrganization :: T.Text
targetOrganization = "pytorch"


githubAuthTokenSessionKey :: String
githubAuthTokenSessionKey = "github_api_token"


getAuthenticatedUser ::
     Request
  -> Vault.Key (Session IO String String)
  -> AuthConfig.GithubConfig
  -> (AuthStages.Username -> IO a)
  -> IO (Either AuthStages.AuthenticationFailureStage a)
getAuthenticatedUser rq session github_config callback = do

  u <- sessionLookup githubAuthTokenSessionKey

  case u of
    Nothing -> return $ Left $ AuthStages.FailLoginRequired
    Just api_token -> do

      mgr <- newManager tlsManagerSettings
      let wrapped_token = OAuth2.AccessToken $ T.pack api_token
          api_support_data = GitHubApiSupport mgr wrapped_token

      either_user <- Auth.fetchUser api_support_data
      case either_user of
        Left _some_text -> return $ Left AuthStages.FailUsernameDetermination
        Right (Types.LoginUser _login_name login_alias) -> do

          let username_text = TL.toStrict login_alias
          either_is_org_member <- Auth.isOrgMember (AuthConfig.personal_access_token github_config) username_text
          case either_is_org_member of
            Left org_membership_determination_err -> return $ Left $ AuthStages.FailMembershipDetermination org_membership_determination_err
            Right is_org_member -> if is_org_member
              then do
                callback_result <- callback $ AuthStages.Username username_text
                return $ Right callback_result

              else return $ Left $ AuthStages.FailOrgMembership (AuthStages.Username username_text) Auth.targetOrganization

  where
    Just (sessionLookup, _sessionInsert) = Vault.lookup session (vault rq)


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
        idp = Github.Github


data GitHubApiSupport = GitHubApiSupport {
    tls_manager  :: Manager
  , access_token :: OAuth2.AccessToken
  }


tryFetchUser ::
     AuthConfig.GithubConfig
  -> TL.Text           -- ^ code
  -> (String -> IO ())
  -> IO (Either TL.Text LoginUser)
tryFetchUser github_config code session_insert = do
  mgr <- newManager tlsManagerSettings
  token <- OAuth2.fetchAccessToken mgr (Keys.githubKey github_config) (OAuth2.ExchangeToken $ TL.toStrict code)

  case token of
    Right at -> do
      let access_token_object = OAuth2.accessToken at
          access_token_string = T.unpack $ OAuth2.atoken access_token_object

      liftIO $ session_insert access_token_string
      fetchUser $ GitHubApiSupport mgr access_token_object

    Left e   -> return $ Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e


getFailedStatuses ::
     T.Text
  -> DbHelpers.OwnerAndRepo
  -> T.Text
  -> IO (Either TL.Text [StatusEvent.GitHubStatusEventSetter])
getFailedStatuses
    token
    (DbHelpers.OwnerAndRepo repo_owner repo_name)
    target_sha1 = do

  mgr <- newManager tlsManagerSettings

  case either_uri of
    Left x -> return $ Left $ "Bad URL: " <> TL.pack uri_string
    Right uri -> do
      r <- OAuth2.authGetJSON mgr (OAuth2.AccessToken token) uri

      return $ first displayOAuth2Error $
        second (filter_failed . Webhooks._statuses) r

  where
    filter_failed = filter $ (== "failure") . StatusEvent._state
    either_uri = parseURI strictURIParserOptions $ BSU.pack uri_string
    uri_string = intercalate "/" [
        "https://api.github.com/repos"
      , repo_owner
      , repo_name
      , "commits"
      , T.unpack target_sha1
      , "status"
      ]


fetchUser :: GitHubApiSupport -> IO (Either TL.Text LoginUser)
fetchUser (GitHubApiSupport mgr token) = do
  re <- do
    r <- OAuth2.authGetJSON mgr token Github.userInfoUri
    return $ second Github.toLoginUser r

  return (first displayOAuth2Error re)


displayOAuth2Error :: OAuth2.OAuth2Error Errors -> TL.Text
displayOAuth2Error = TL.pack . show


-- | The Github API for this returns an empty response, using
-- status codes 204 or 404 to represent success or failure, respectively.
isOrgMemberInner :: OAuth2.OAuth2Result TL.Text LBS.ByteString -> Bool
isOrgMemberInner either_response = case either_response of
  Left (OAuth2.OAuth2Error _either_parsed_err _maybe_description _maybe_uri) -> False
  Right _ -> True


-- | Alternate (user-centric) API endpoint is:
-- https://developer.github.com/v3/orgs/members/#get-your-organization-membership
isOrgMember :: T.Text -> T.Text -> IO (Either T.Text Bool)
isOrgMember personal_access_token username = do
  mgr <- newManager tlsManagerSettings

  -- Note: This query is currently using a Personal Access Token from a pytorch org member.
  -- TODO This must be converted to an App token.
  let api_support_data = GitHubApiSupport mgr wrapped_token

  case either_membership_query_uri of
    Left x -> return $ Left $ "Bad URL: " <> url_string
    Right membership_query_uri -> do
      either_response <- OAuth2.authGetBS mgr wrapped_token membership_query_uri
      return $ Right $ isOrgMemberInner either_response
  where
    wrapped_token = OAuth2.AccessToken personal_access_token
    url_string = "https://api.github.com/orgs/" <> targetOrganization <> "/members/" <> username
    either_membership_query_uri = parseURI strictURIParserOptions $ BSU.pack $ T.unpack url_string
