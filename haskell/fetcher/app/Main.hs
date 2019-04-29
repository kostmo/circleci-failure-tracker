import           Control.Concurrent  (getNumCapabilities)
import           Options.Applicative
import           System.IO

import qualified BuildRetrieval
import qualified DbHelpers
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite


data CommandLineArgs = NewCommandLineArgs {
    buildCount :: Int
  , ageDays    :: Int
  , dbHostname :: String
  , quiet      :: Bool
    -- ^ Suppress console output
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "count"       <> value 3           <> metavar "BUILD_COUNT"
    <> help "Maximum number of failed builds to fetch from CircleCI")
  <*> option auto (long "age"         <> value 365         <> metavar "AGE_DAYS"
    <> help "Maximum age of build to fetch from CircleCI")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> switch      (long "quiet"
    <> help "Suppress console output")


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering

  capability_count <- getNumCapabilities
  print $ "Num capabilities: " ++ show capability_count

  conn <- SqlWrite.prepare_database

  BuildRetrieval.updateBuildsList conn fetch_count age_days
  unvisited_builds_list <- SqlRead.get_unvisited_build_ids conn fetch_count

  putStrLn "Storing build failure metadata..."
  scan_resources <- Scanning.prepare_scan_resources conn
  Scanning.process_builds scan_resources unvisited_builds_list

  where
    fetch_count = buildCount args
    age_days = ageDays args


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Scans CircleCI failure logs"
     <> header "fetcher - performs the scan, populates database" )

