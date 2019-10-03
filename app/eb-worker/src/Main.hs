{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Maybe                                      as Maybe
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import qualified Data.Vault.Lazy                                 as Vault
import           Database.PostgreSQL.PQTypes.Internal.Connection (ConnectionSettings (ConnectionSettings),
                                                                  simpleSource,
                                                                  unConnectionSource)
import           Log.Backend.PostgreSQL                          (withPgLogger)
import           Log.Monad                                       (runLogT)
import qualified Network.OAuth.OAuth2                            as OAuth2
import           Network.Wai.Session.ClientSession               (clientsessionStore)
import           Options.Applicative
import           System.Environment                              (lookupEnv)
import           Text.Read                                       (readMaybe)
import qualified Text.URI                                        as URI
import           Web.ClientSession                               (getDefaultKey)
import qualified Web.Scotty                                      as S

import qualified AuthConfig
import qualified DbHelpers
import qualified Routes
import qualified Session


data CommandLineArgs = NewCommandLineArgs {
    serverPort                :: Int
  , staticBase                :: String
  , dbHostname                :: String
  , dbUsername                :: String
  , dbPassword                :: String
  , dbMviewUsername           :: String
  , dbMviewPassword           :: String
  , gitHubClientID            :: Text
  , gitHubClientSecret        :: Text
  , gitHubPersonalAccessToken :: Text
  , gitHubWebhookSecret       :: Text
  , runningLocally            :: Bool
  , adminPassword             :: Text
  , noForceSSL                :: Bool
  }


getPostgresLoggingUri info = do

  postgres_logging_user_info <- URI.UserInfo
    <$> URI.mkUsername (T.pack $ DbHelpers.dbUsername info)
    <*> (Just <$> URI.mkPassword (T.pack $ DbHelpers.dbPassword info))

  postgres_logging_host <- URI.mkHost (T.pack $ DbHelpers.dbHostname info)

  let postgres_logging_authority = URI.Authority
        (Just postgres_logging_user_info)
        postgres_logging_host
        (Just 5432)

  postgres_logging_dbname <- URI.mkPathPiece $ T.pack $ DbHelpers.dbName info

  postgres_logging_scheme <- URI.mkScheme "postgres"

  return $ URI.URI
    (Just postgres_logging_scheme)
    (Right postgres_logging_authority)
    (Just (False, pure postgres_logging_dbname))
    []
    Nothing


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe (serverPort args) $ readMaybe =<< maybe_envar_port

  postgres_logging_connection_uri <- getPostgresLoggingUri connection_data


  with_logger postgres_logging_connection_uri $ \logger ->

    S.scotty prt $ Routes.scottyApp
      (runLogT "dr-ci-eb-worker" logger)
      credentials_data

  where
    with_logger logging_db_uri = withPgLogger "frontend_logging.logs" $
      unConnectionSource $ simpleSource $ ConnectionSettings
        (URI.render logging_db_uri)
        Nothing
        []

    credentials_data = Routes.SetupData
      static_base
      github_config
      connection_data
      connection_data_mview

    static_base = staticBase args

    access_token = OAuth2.AccessToken $ gitHubPersonalAccessToken args
    github_config = AuthConfig.NewGithubConfig
      (runningLocally args)
      (gitHubClientID args)
      (gitHubClientSecret args)
      access_token
      (gitHubWebhookSecret args)
      (adminPassword args)
      (noForceSSL args)

    databaseName = "loganci"

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = databaseName
      , DbHelpers.dbUsername = dbUsername args
      , DbHelpers.dbPassword = dbPassword args
      }

    connection_data_mview = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = databaseName
      , DbHelpers.dbUsername = dbMviewUsername args
      , DbHelpers.dbPassword = dbMviewPassword args
      }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "port"      <> value 3001           <> metavar "PORT"
    <> help "Webserver port")
  <*> strOption   (long "data-path" <> value "/data/static" <> metavar "STATIC_DATA"
    <> help "Path to static data files")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")

  <*> strOption   (long "db-username" <> metavar "DATABASE_USER"
    <> help "Username for database user")
  <*> strOption   (long "db-password" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")

  <*> strOption   (long "db-mview-username" <> metavar "DATABASE_MVIEW_USER"
    <> help "Username for materialized views database user")
  <*> strOption   (long "db-mview-password" <> metavar "DATABASE_MVIEW_PASSWORD"
    <> help "Password for materialized views database user")

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
  <*> switch      (long "no-force-ssl"
    <> help "Do not redirect HTTP to HTTPS")

main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "CircleCI failure log analysis webserver"
     <> header "webapp - user frontend" )
