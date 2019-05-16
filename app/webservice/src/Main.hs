{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                   (unless)
import           Control.Monad.IO.Class          (liftIO)
import           Data.ByteString                 (ByteString)
import qualified Data.Text.Encoding                 as TE
import qualified Data.Text.Lazy                 as LT
import           Data.List                       (filter)
import           Data.List.Split                 (splitOn)
import qualified Data.Maybe                      as Maybe
import           Data.SecureMem
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.ByteString.Char8 as BSU
import           Data.Text.Encoding              (encodeUtf8)
import qualified Data.Text.Internal.Lazy         as LT
import           Network.Wai                     (Request, pathInfo)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.HttpAuth
import           Network.Wai.Middleware.Static
import           Options.Applicative
import           System.Environment              (lookupEnv)
import           System.FilePath
import           Text.Read                       (readMaybe)
import qualified Web.Scotty                      as S
import qualified Web.Scotty.Internal.Types       as ScottyTypes
import Network.Wai.Session (SessionStore)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import qualified Network.OAuth.OAuth2.Internal as OAuth2
import Network.OAuth.OAuth2.HttpClient (authGetBS)

import qualified Builds
import qualified DbHelpers
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified WebApi

import qualified Auth
import qualified AuthConfig
import qualified IDP
import qualified Session
import qualified Types

import Data.Time

import Data.Default (def)
import Data.String (fromString)
import qualified Data.Vault.Lazy as Vault

import Network.Wai
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)
import Web.ClientSession (getDefaultKey)


githubAuthTokenSessionKey = "github_api_token"


pattern_from_parms :: ScottyTypes.ActionT LT.Text IO ScanPatterns.Pattern
pattern_from_parms = do

  expression <- S.param "pattern"
  is_regex_str <- S.param "is_regex"
  description <- S.param "description"
  tags <- S.param "tags"
  applicable_steps <- S.param "applicable_steps"

  let is_regex = is_regex_str == ("true" :: Text)
      match_expression = if is_regex
        then ScanPatterns.RegularExpression $ encodeUtf8 expression
        else ScanPatterns.LiteralExpression expression

  return $ ScanPatterns.NewPattern
    match_expression
    description
    (listify tags)
    (listify applicable_steps)
    1
  where
    listify = filter (not . T.null) . map (T.strip . T.pack) . splitOn ","


data SetupData = SetupData {
    setup_static_base :: String
  , setup_github_config :: AuthConfig.GithubConfig
  , setup_connection_data :: DbHelpers.DbConnectionData
  }



data PersistenceData = PersistenceData {
    setup_cache :: Types.CacheStore
  , setup_session :: Vault.Key (Session IO String String)
  , setup_store :: SessionStore IO String String
  }


scottyApp :: PersistenceData -> SetupData -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp (PersistenceData cache session store) (SetupData static_base github_config connection_data) = do

    S.middleware $ withSession store (fromString "SESSION") def session

    S.middleware $ staticPolicy (noDots >-> addBase static_base)

    unless (AuthConfig.is_local github_config) $
      S.middleware $ forceSSL



    S.get "/login" $ Auth.indexH cache

    -- XXX IMPORTANT:
    -- The session cookie is specific to the parent dir of the path.
    -- So with the path "/api/callback", only HTTP accesses to paths
    -- at or below the "/api/" path will be members of the same session.
    -- Consequentially, a cookie set (namely, the github access token)
    -- in a request to a certain path will only be accessible to
    -- other requests at or below that same parent directory.
    S.get "/api/github-auth-callback" $ do
      rq <- S.request
      let Just (sessionLookup, sessionInsert) = Vault.lookup session (vault rq)
      Auth.callbackH cache github_config (sessionInsert githubAuthTokenSessionKey)

    S.get "/logout" $ Auth.logoutH cache

    S.get "/api/failed-commits-by-day" $
      S.json =<< liftIO (SqlRead.api_failed_commits_by_day connection_data)

    S.get "/api/job" $
      S.json =<< liftIO (SqlRead.api_jobs connection_data)

    S.get "/api/log-size-histogram" $
      S.json =<< liftIO (SqlRead.api_line_count_histogram connection_data)

    S.get "/api/step" $
      S.json =<< liftIO (SqlRead.api_step connection_data)

    S.get "/api/new-pattern-test" $ do
      buildnum_str <- S.param "build_num"
      new_pattern <- pattern_from_parms
      S.json =<< (liftIO $ SqlWrite.api_new_pattern_test (Builds.NewBuildNumber $ read buildnum_str) new_pattern)


    -- TODO
    S.post "/api/github-event" $ do
      S.json =<< return ["hello" :: String]


    -- TODO Flatten this via Either monad
    S.post "/api/new-pattern-insert" $ do

      liftIO $ putStrLn "Trying to insert a pattern..."
      rq <- S.request
      let Just (sessionLookup, sessionInsert) = Vault.lookup session (vault rq)

      u <- liftIO $ sessionLookup githubAuthTokenSessionKey

      case u of
        Just api_token -> do
          new_pattern <- pattern_from_parms
          web_response <- liftIO $ do

            mgr <- newManager tlsManagerSettings
            let wrapped_token = OAuth2.AccessToken $ T.pack api_token
                api_support_data = Auth.GitHubApiSupport mgr wrapped_token

            either_user <- Auth.fetchUser api_support_data
            case either_user of
              Left _some_text -> return $ WebApi.FailResult WebApi.InsertionFailAuthentication "Could not determine username."
              Right (Types.LoginUser login_name login_alias) -> do

                either_is_org_member <- Auth.isOrgMember (AuthConfig.personal_access_token github_config) $ LT.toStrict login_alias
                case either_is_org_member of
                  Left org_membership_determination_err -> return $ WebApi.FailResult WebApi.InsertionFailAuthentication $ "Could not determine org membership: " <> T.unpack 	org_membership_determination_err
                  Right is_org_member -> if is_org_member
                    then do
                      insertion_result <- liftIO $ SqlWrite.api_new_pattern connection_data (LT.toStrict login_alias) new_pattern
                      return $ insertion_result
                    else return $ WebApi.FailResult WebApi.InsertionFailAuthentication $ "User \"" <> LT.unpack login_alias <> "\" is not a member of the organization \"" <> T.unpack Auth.targetOrganization <> "\"!"

          S.json $ WebApi.toInsertionResponse web_response

        Nothing -> do
          liftIO $ putStrLn "Token not found!"
          S.json $ WebApi.toInsertionResponse $
            WebApi.FailResult WebApi.InsertionFailAuthentication "token lookup failed; you need to log in!"


    S.get "/api/tag-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_tags connection_data term)

    S.get "/api/step-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_steps connection_data term)

    S.get "/api/branch-suggest" $ do
      term <- S.param "term"
      S.json =<< (liftIO $ SqlRead.api_autocomplete_branches connection_data term)

    S.get "/api/random-scannable-build" $
      S.json =<< liftIO (SqlRead.api_random_scannable_build connection_data)

    S.get "/api/summary" $
      S.json =<< liftIO (SqlRead.api_summary_stats connection_data)

    S.get "/api/unmatched-builds" $
      S.json =<< liftIO (SqlRead.api_unmatched_builds connection_data)

    S.get "/api/idiopathic-failed-builds" $
      S.json =<< liftIO (SqlRead.api_idiopathic_builds connection_data)

    S.get "/api/disk" $ do
      S.json =<< liftIO WebApi.api_disk_space

    S.get "/api/pattern" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.api_single_pattern connection_data $ read pattern_id)

    S.get "/api/patterns" $ do
      S.json =<< liftIO (SqlRead.api_patterns connection_data)

    S.get "/api/patterns-branch-filtered" $ do
      branches <- S.param "branches"
      liftIO $ putStrLn $ "Got branch list: " ++ show branches
      S.json =<< liftIO (SqlRead.api_patterns_branch_filtered connection_data branches)

    S.get "/api/single-build-info" $ do
      build_id <- S.param "build_id"
      S.json =<< (liftIO $ SqlRead.get_build_info connection_data build_id)

    S.get "/api/build-pattern-matches" $ do
      build_id <- S.param "build_id"
      S.json =<< (liftIO $ SqlRead.get_build_pattern_matches connection_data build_id)

    S.get "/api/best-pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.get_best_pattern_matches connection_data pattern_id)

    S.get "/api/pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      S.json =<< (liftIO $ SqlRead.get_pattern_matches connection_data pattern_id)

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file $ static_base </> "images/favicon.ico"

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file $ static_base </> "index.html"


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe (serverPort args) $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  cache <- Session.initCacheStore
  IDP.initIdps cache github_config

  session <- Vault.newKey
  store <- fmap clientsessionStore getDefaultKey
  
  S.scotty prt $ scottyApp (PersistenceData cache session store) credentials_data

  where

    credentials_data = SetupData static_base github_config connection_data
    static_base = staticBase args

    github_config = AuthConfig.GithubConfig
      (runningLocally args)
      (gitHubClientID args)
      (gitHubClientSecret args)
      (gitHubPersonalAccessToken args)

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = "logan"
      , DbHelpers.dbPassword = dbPassword args
      }


data CommandLineArgs = NewCommandLineArgs {
    serverPort         :: Int
  , staticBase         :: String
  , dbHostname         :: String
  , dbPassword         :: String
  , gitHubClientID     :: Text
  , gitHubClientSecret :: Text
  , gitHubPersonalAccessToken :: Text
  , runningLocally     :: Bool
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "port"       <> value 3000           <> metavar "PORT"
    <> help "Webserver port")
  <*> strOption   (long "data-path" <> value "/data/static" <> metavar "STATIC_DATA"
    <> help "Path to static data files")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> value "logan01" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
   -- Note: this is not the production password; this default is only for local testing
  <*> strOption   (long "github-client-id" <> metavar "GITHUB_CLIENT_ID"
    <> help "Client ID for GitHub app")
  <*> strOption   (long "github-client-secret" <> metavar "GITHUB_CLIENT_SECRET"
    <> help "Client secret for GitHub app")
  <*> strOption   (long "github-personal-access-token" <> metavar "GITHUB_PERSONAL_ACCESS_TOKEN"
    <> help "For debugging purposes. This will be removed eventually/")
  <*> switch      (long "local"
    <> help "Webserver is being run locally, so don't redirect HTTP to HTTPS")


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "CircleCI failure log analsys webserver"
     <> header "webapp - user frontend" )
