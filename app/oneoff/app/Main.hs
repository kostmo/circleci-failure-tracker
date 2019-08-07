{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent   (getNumCapabilities)
import qualified Data.Maybe           as Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Network.OAuth.OAuth2 as OAuth2
import           Options.Applicative

import           System.IO

import qualified BuildRetrieval
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified MergeBase
import qualified Scanning
import qualified SqlRead
import qualified SqlUpdate


data CommandLineArgs = NewCommandLineArgs {
    dbHostname                :: String
  , dbPassword                :: String
  , rescanVisited             :: Bool
  , gitHubPersonalAccessToken :: Text
  , repoGitDir                :: FilePath
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
  <*> strOption   (long "github-personal-access-token" <> metavar "GITHUB_PERSONAL_ACCESS_TOKEN"
    <> help "For debugging purposes. This will be removed eventually")
  <*> strOption   (long "repo-git-dir" <> metavar "GIT_DIR"
    <> help "Path to .git directory of repository, for use in computing merge bases")



mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering



  mb_result <- MergeBase.gitMergeBase
    (repoGitDir args)
    (Builds.RawCommit "145d809db2a52267b32f0e56f02607307a39532")

  putStrLn $ unwords [
      "MB result:"
    , show mb_result
    ]



  conn <- DbPreparation.prepareDatabase connection_data False


  -- Experimental
  scan_resources <- Scanning.prepareScanResources conn $ Just Constants.defaultPatternAuthor

  {-
  build_failure_result <- Scanning.getCircleCIFailedBuildInfo scan_resources $ Builds.NewBuildNumber 2340371
  putStrLn $ "Build failure retrieval result: " ++ show build_failure_result
  -}

  all_master_commits <- SqlRead.getAllMasterCommits conn



  {-
  result <- SqlRead.findMasterAncestorWithPrecomputation
    (Just all_master_commits)
    conn
    oauth_access_token
    owned_repo
    (Builds.RawCommit "9588cd921efd051a982188220b8215914bd5790e")

  putStrLn $ unwords [
      "Ancestor finding result:"
    , show result
    ]
  -}


  batch_diagnosis_result <- SqlUpdate.diagnoseCommitsBatch
    (Just $ repoGitDir args)
    conn
    oauth_access_token
    owned_repo
    {-
    [ Builds.RawCommit "60a4ef30747ef3fcc780d4327e7b44de0a330b2d" -- (master) has no breakage
    , Builds.RawCommit "43c4bcba1d0b336736ce14e939907c47834d32c7" -- (master) has a known breakage

    , Builds.RawCommit "8145d809db2a52267b32f0e56f02607307a39532" -- (PR) has no breakage
--    , Builds.RawCommit "" -- (PR) has a known breakage
    ]
    -}

  putStrLn $ unwords [
      "Batch diagnosis result:"
    , show batch_diagnosis_result
    ]



  return ()

  {-

  either_logstore_result <- Scanning.getAndStoreLog
    scan_resources
    True
    (Builds.NewBuildNumber 2101460)
    (Builds.NewBuildStepId 10876)
    Nothing

--  putStrLn $ "logstore result: " ++ show either_logstore_result



  maybe_log <- SqlRead.readLog conn $ Builds.NewBuildNumber 2101460
  putStrLn $ "Log output: " <> Maybe.fromMaybe "<no log>" (Scanning.filterAnsi . T.unpack <$> maybe_log)
  -}

--  putStrLn $ "Log linecount: " <> show (maybe 0 (length . T.lines) maybe_log)
  where
    owned_repo = DbHelpers.OwnerAndRepo Constants.project_name Constants.repo_name

    oauth_access_token = OAuth2.AccessToken $ gitHubPersonalAccessToken args

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

