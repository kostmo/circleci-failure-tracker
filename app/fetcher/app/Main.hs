import           Control.Concurrent  (getNumCapabilities)
import           Options.Applicative
import           System.IO

import qualified BuildRetrieval
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified Scanning


data CommandLineArgs = NewCommandLineArgs {
    buildCount    :: Int
  , ageDays       :: Int
  , branchName    :: [String]
  , dbHostname    :: String
  , dbPassword    :: String
  , wipeDatabase  :: Bool
  , rescanVisited :: Bool
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "count"       <> value 3           <> metavar "BUILD_COUNT"
    <> help "Maximum number of failed builds to fetch from CircleCI")
  <*> option auto (long "age"         <> value 365         <> metavar "AGE_DAYS"
    <> help "Maximum age of build to fetch from CircleCI")
  <*> some (strOption   (long "branch" <> metavar "BRANCH_NAME"
    <> help "Branch name (can specify multiple)"))
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> value "logan01" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
   -- Note: this is not the production password; this default is only for local testing
  <*> switch      (long "wipe"
    <> help "Wipe database content before beginning")
  <*> switch      (long "rescan"
    <> help "Rescan previously visited builds")


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering

  capability_count <- getNumCapabilities
  putStrLn $ unwords ["Num capabilities:", show capability_count]

  conn <- DbPreparation.prepareDatabase connection_data $ wipeDatabase args


  -- Experimental
  {-
  my_scan_result <- Scanning.getCircleCIFailedBuildInfo scan_resources $ Builds.NewBuildNumber 2102088
  putStrLn $ "My scan result: " ++ show my_scan_result
  -}


  BuildRetrieval.updateCircleCIBuildsList conn (branchName args) fetch_count age_days


  scan_resources <- Scanning.prepareScanResources conn $ Just Constants.defaultPatternAuthor


  build_matches <- Scanning.scanBuilds
    scan_resources
    (rescanVisited args)
    False -- Do not re-download log
    (Right fetch_count)

  putStrLn $ unwords ["Scanned", show $ length build_matches, "builds."]

  where
    fetch_count = buildCount args
    age_days = ageDays args

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
     <> progDesc "Scans CircleCI failure logs"
     <> header "fetcher - performs the scan, populates database" )

