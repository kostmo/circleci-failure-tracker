{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              (void, when)

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Either                (fromRight)
import qualified Data.Text.Lazy             as LT
import           Options.Applicative
import           System.IO

import qualified Builds
import qualified CommitBuilds
import qualified DbHelpers
import qualified DbPreparation
import qualified DebugUtils                 as D
import qualified GadgitTest
import qualified MatchOccurrences
import qualified Sql.Read.Read              as SqlRead
import qualified TestDefs
import qualified TestHelpers


myCliParser :: Parser TestHelpers.CommandLineArgs
myCliParser = TestHelpers.NewCommandLineArgs
  <$> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
  <*> strOption   (long "db-password" <> value "logan01" <> metavar "DATABASE_PASSWORD"
    <> help "Password for database user")
   -- Note: this is not the production password; this default is only for local testing
  <*> strOption   (long "circleci-api-token" <> metavar "CIRCLECI_API_TOKEN"
    <> help "CircleCI API token for triggering rebuilds")
  <*> switch      (long "rescan"
    <> help "Rescan previously visited builds")
  <*> strOption   (long "github-app-rsa-pem" <> metavar "GITHUB_APP_RSA_PEM"
    <> help "GitHub App PEM file content")
  <*> strOption   (long "aws-sqs-queue-url" <> metavar "AWS_SQS_QUEUE_URL"
    <> help "AWS SQS queue URL for commit SHA1 processing")
  <*> strOption   (long "repo-git-dir" <> metavar "GIT_DIR"
    <> help "Path to .git directory of repository, for use in computing merge bases")


mainAppCode :: TestHelpers.CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering

  conn <- DbPreparation.prepareDatabase connection_data False

  when False $
    void $ TestDefs.benchmarkScan conn args $ Builds.UniversalBuildId 82047188

  when False $
    void $ TestDefs.testGetSpanningBreakages conn args $
      Builds.RawCommit "c1fa71972e9cdc50562d6f807fc6ca893c585112"

  when True $
    void $ TestHelpers.testWithAccessToken args $ TestDefs.testBotCommentGeneration args conn $
--      Builds.RawCommit "c2e1496377685db6cd584aec37b9f7ec48992770"
      Builds.RawCommit "440acd7ab3276327f23075c5ba5987b2f98ba629"



  when False $
    void $ TestDefs.testGitHubStatusesRetrieval conn args $ Builds.RawCommit
      "8e80f56fc61e68d6fe3715482a961be1f545ea2b"

  when False $
    void $ TestDefs.testGitHubChecksRetrieval conn args $ Builds.RawCommit
      "8e80f56fc61e68d6fe3715482a961be1f545ea2b"




  when False $ do
    foo <- flip runReaderT conn $ SqlRead.logContextFunc 0
      (MatchOccurrences.MatchId 7113010)
      10

    D.debugStr "BLARG LINES"
    let f (x, y) = unwords [show x ++ ":", LT.unpack $ LT.strip y]
    D.debugStr $ (unlines . map f . CommitBuilds._log_lines) $ fromRight (error "BAD") foo

  when False $ do
    TestDefs.testTestRetrieval conn args $ Builds.UniversalBuildId 110871872
    return ()


  when False $
    GadgitTest.testGadgitApis


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

  where
    connection_data = DbHelpers.NewDbConnectionData {
        DbHelpers.dbHostname = TestHelpers.dbHostname args
      , DbHelpers.dbName = "loganci"
      , DbHelpers.dbUsername = "logan"
      , DbHelpers.dbPassword = TestHelpers.dbPassword args
      }


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Test script"
     <> header "experimental" )

