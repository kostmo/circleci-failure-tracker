{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Either                (fromRight)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Network.OAuth.OAuth2       as OAuth2
import           Options.Applicative
import           System.IO

import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified DebugUtils                 as D
import qualified GadgitFetch
import qualified GadgitTest
import qualified GitRev
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusUpdate


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


  {-
  let commit_sha1_text = "1c6d7505adf51db267a0b27724028fb0c73ecbdd"
      raw_commit = Builds.RawCommit commit_sha1_text
      validated_sha1 = fromRight (error "BAD") $ GitRev.validateSha1 commit_sha1_text

  blah <- runReaderT (SqlRead.getRevisionBuilds validated_sha1) conn
  let DbHelpers.BenchmarkedResponse _ revision_builds = fromRight (error "BAD2") blah

  commit_page_info <- liftIO $
    runReaderT (StatusUpdate.fetchCommitPageInfo raw_commit validated_sha1) conn

  putStrLn $ T.unpack $ T.unlines $ StatusUpdate.genBuildFailuresTable $
    fromRight (error "BAD3") commit_page_info

  -}

  putStrLn "============================="


  GadgitTest.testGadgitApis

  putStrLn "============================="


  {-

  batch_diagnosis_result <- SqlUpdate.diagnoseCommitsBatch
    (Just git_repo_dir)
    conn
    oauth_access_token
    owned_repo

  putStrLn $ unwords [
      "Batch diagnosis result:"
    , show batch_diagnosis_result
    ]

  -}

  return ()

  where
    git_repo_dir = repoGitDir args
    owned_repo = DbHelpers.OwnerAndRepo Constants.projectName Constants.repoName

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

