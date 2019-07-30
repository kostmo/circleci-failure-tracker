{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent  (getNumCapabilities)
import qualified Data.Maybe          as Maybe
import qualified Data.Text           as T
import           Options.Applicative
import           System.IO

import qualified BuildRetrieval
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified Scanning
import qualified SqlRead


data CommandLineArgs = NewCommandLineArgs {
    dbHostname    :: String
  , dbPassword    :: String
  , rescanVisited :: Bool
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> value "logan01" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
   -- Note: this is not the production password; this default is only for local testing
  <*> switch      (long "rescan"
    <> help "Rescan previously visited builds")


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering

  conn <- DbPreparation.prepare_database connection_data False


  -- Experimental

  scan_resources <- Scanning.prepareScanResources conn $ Just Constants.defaultPatternAuthor

  my_scan_result <- Scanning.getCircleCIFailedBuildInfo scan_resources $ Builds.NewBuildNumber 2102088
  putStrLn $ "My scan result: " ++ show my_scan_result


  either_logstore_result <- Scanning.getAndStoreLog
    scan_resources
    True
    (Builds.NewBuildNumber 2101460)
    (Builds.NewBuildStepId 10876)
    Nothing

--  putStrLn $ "logstore result: " ++ show either_logstore_result



  maybe_log <- SqlRead.readLog conn $ Builds.NewBuildNumber 2101460
  putStrLn $ "Log output: " <> Maybe.fromMaybe "<no log>" (Scanning.filterAnsi . T.unpack <$> maybe_log)

--  putStrLn $ "Log linecount: " <> show (maybe 0 (length . T.lines) maybe_log)
  where

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = "logan"
      , DbHelpers.dbPassword = dbPassword args
      }


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Test script"
     <> header "experimental" )

