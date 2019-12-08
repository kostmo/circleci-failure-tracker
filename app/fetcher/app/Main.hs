import           Control.Concurrent  (getNumCapabilities)
import           Options.Applicative
import           System.IO

import qualified BuildRetrieval
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified DebugUtils          as D
import qualified Scanning


data CommandLineArgs = NewCommandLineArgs {
    buildCount    :: Int
  , branchName    :: [String]
  , dbHostname    :: String
  , dbUsername    :: String
  , dbPassword    :: String
  , wipeDatabase  :: Bool
  , rescanVisited :: Bool
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "count"       <> value 3           <> metavar "BUILD_COUNT"
    <> help "Maximum number of failed builds to fetch from CircleCI")
  <*> some (strOption   (long "branch" <> metavar "BRANCH_NAME"
    <> help "Branch name (can specify multiple)"))
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-username" <> value "logan" <> metavar "DATABASE_USER"
    <> help "Username for database user")
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
  D.debugList [
      "Num capabilities:"
    , show capability_count
    ]

  conn <- DbPreparation.prepareDatabase connection_data $ wipeDatabase args

  BuildRetrieval.updateCircleCIBuildsList
    conn
    Nothing
    BuildRetrieval.Completed
    0
    (branchName args)
    fetch_count

  scan_resources <- Scanning.prepareScanResources conn $
    Just Constants.defaultPatternAuthor

  build_matches <- Scanning.scanBuilds
    scan_resources
    (if rescanVisited args then Scanning.RevisitScanned else Scanning.NoRevisit)
    Scanning.NoRefetchLog
    (Right fetch_count)

  D.debugList [
      "Scanned"
    , show $ length build_matches
    , "builds."
    ]

  where
    fetch_count = buildCount args

    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = dbUsername args
      , DbHelpers.dbPassword = dbPassword args
      }


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Scans CircleCI failure logs"
     <> header "fetcher - performs the scan, populates database" )

