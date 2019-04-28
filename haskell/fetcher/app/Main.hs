import           Control.Concurrent  (getNumCapabilities)
import           Options.Applicative

import qualified DbHelpers
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite


data CommandLineArgs = NewCommandLineArgs {
    buildCount :: Int
  , ageDays    :: Int
  , title      :: String
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

  conn <- DbHelpers.get_connection
  SqlWrite.scrub_tables conn

  SqlWrite.populate_patterns conn ScanPatterns.pattern_list

  capability_count <- getNumCapabilities
  print $ "Num capabilities: " ++ show capability_count


  -- TODO: Handle network exceptions: https://stackoverflow.com/a/48365179/105137

  putStrLn "Fetching builds list..."
  downloaded_builds_list <- Scanning.populate_builds fetch_count age_days

  putStrLn "Storing builds list..."
  SqlWrite.store_builds_list conn downloaded_builds_list

  pattern_records <- SqlRead.get_patterns conn
  let patterns_by_id = DbHelpers.to_dict pattern_records
  scannable_build_patterns <- SqlRead.get_unscanned_build_patterns conn patterns_by_id

  unvisited_builds_list <- SqlRead.get_unvisited_build_ids conn fetch_count

  putStrLn "Storing build failure metadata..."
  Scanning.store_build_failure_metadata conn unvisited_builds_list

  putStrLn "Scanning logs..."
  _match_count <- Scanning.scan_all_logs conn scannable_build_patterns

  build_list <- SqlRead.query_builds
  print $ "Build count: " ++ show (length build_list)

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

