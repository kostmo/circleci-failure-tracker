{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent   (getNumCapabilities)
import qualified Data.Maybe           as Maybe
import qualified Data.Set             as Set
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
import qualified SqlWrite


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



  conn <- DbPreparation.prepareDatabase connection_data False




  -- Experimental
  scan_resources <- Scanning.prepareScanResources conn $ Just Constants.defaultPatternAuthor



  {-
  -- This is a HUGE log
  universal_build <- Scanning.reInsertCircleCiBuild
    conn
    (Builds.NewBuildNumber 2120936)
    (Builds.RawCommit "b5ebb30cd93db81763c9a2f5f659a9b25841f035")
    False
  -}




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

