{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                     (when)
import qualified Data.Maybe                        as Maybe
import           Data.Text                         (Text)
import qualified Data.Vault.Lazy                   as Vault
import qualified Network.OAuth.OAuth2              as OAuth2
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           Options.Applicative
import           System.Environment                (lookupEnv)
import           Text.Read                         (readMaybe)
import           Web.ClientSession                 (getDefaultKey)
import qualified Web.Scotty                        as S

import qualified AuthConfig
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified Routes
import qualified Session
import qualified SqlRead


data CommandLineArgs = NewCommandLineArgs {
    serverPort                :: Int
  , staticBase                :: String
  , dbHostname                :: String
  , dbPassword                :: String
  , gitHubClientID            :: Text
  , gitHubClientSecret        :: Text
  , gitHubPersonalAccessToken :: Text
  , gitHubWebhookSecret       :: Text
  , runningLocally            :: Bool
  , adminPassword             :: Text
  }


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe (serverPort args) $ readMaybe =<< maybe_envar_port

  -- TODO get rid of this?
  cache <- Session.initCacheStore
  AuthConfig.initIdps cache github_config

  session <- Vault.newKey
  store <- fmap clientsessionStore getDefaultKey

  let persistence_data = Routes.PersistenceData cache session store



  when (AuthConfig.is_local github_config) $ do
    -- XXX FOR TESTING ONLY

    conn <- DbHelpers.get_connection connection_data
    latest_pattern_id <- SqlRead.getLatestPatternId conn
    putStrLn $ unwords ["Latest pattern ID:", show latest_pattern_id]

    find_master_ancestor <- SqlRead.findMasterAncestor
      conn
      access_token
      (DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name)
      (Builds.RawCommit "058beae4115afb76ee5f45dfa42c6ab4ee01895c")

    putStrLn $ "Master ancestor: " ++ show find_master_ancestor
  {-
    SqlWrite.populateLatestMasterCommits
      connection_data
      access_token
      (DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name)
  -}
    return ()


  S.scotty prt $ Routes.scottyApp persistence_data credentials_data

  where
    credentials_data = Routes.SetupData static_base github_config connection_data
    static_base = staticBase args

    access_token = OAuth2.AccessToken $ gitHubPersonalAccessToken args
    github_config = AuthConfig.NewGithubConfig
      (runningLocally args)
      (gitHubClientID args)
      (gitHubClientSecret args)
      access_token
      (gitHubWebhookSecret args)
      (adminPassword args)

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = "logan"
      , DbHelpers.dbPassword = dbPassword args
      }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "port"      <> value 3001           <> metavar "PORT"
    <> help "Webserver port")
  <*> strOption   (long "data-path" <> value "/data/static" <> metavar "STATIC_DATA"
    <> help "Path to static data files")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
  <*> strOption   (long "github-client-id" <> metavar "GITHUB_CLIENT_ID"
    <> help "Client ID for GitHub app")
  <*> strOption   (long "github-client-secret" <> metavar "GITHUB_CLIENT_SECRET"
    <> help "Client secret for GitHub app")
  <*> strOption   (long "github-personal-access-token" <> metavar "GITHUB_PERSONAL_ACCESS_TOKEN"
    <> help "For debugging purposes. This will be removed eventually")
  <*> strOption   (long "github-webhook-secret" <> metavar "GITHUB_WEBHOOK_SECRET"
    <> help "GitHub webhook secret")
  <*> switch      (long "local"
    <> help "Webserver is being run locally, so don't redirect HTTP to HTTPS")
  <*> strOption   (long "admin-password" <> metavar "ADMIN_PASSWORD"
    <> help "Admin password")


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "CircleCI failure log analysis webserver"
     <> header "webapp - user frontend" )
