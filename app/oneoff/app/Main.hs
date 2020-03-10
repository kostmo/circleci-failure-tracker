{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as B
import           Data.Either                (fromRight)
import           Data.Foldable              (for_)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Network.OAuth.OAuth2       as OAuth2
import           Options.Applicative
import           System.IO

import qualified AmazonQueue
import qualified AmazonQueueData
import qualified Builds
import qualified CircleApi
import qualified CircleAuth
import qualified CircleTrigger
import qualified CommentRender
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified DebugUtils                 as D
import qualified GadgitFetch
import qualified GadgitTest
import qualified GitRev
import qualified Scanning
import qualified Sql.Read                   as SqlRead
import qualified Sql.Update                 as SqlUpdate
import qualified StatusUpdate
import qualified StatusUpdateTypes


data CommandLineArgs = NewCommandLineArgs {
    dbHostname          :: String
  , dbPassword          :: String
  , circleciApiToken    :: String
  , rescanVisited       :: Bool
  , gitHubAppPemContent :: B.ByteString
  , sqsQueueUrl         :: Text
  , repoGitDir          :: FilePath
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
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


benchmarkScan circle_token conn build_id = runExceptT $ do

  scan_resources <- ExceptT $ Scanning.prepareScanResources
    circle_token
    conn
    Scanning.NoPersist
    Nothing

  liftIO $ Scanning.scanBuilds
    scan_resources
    Scanning.ScanAllPatterns
    Scanning.RevisitScanned
    Scanning.NoRefetchLog
    (Left $ Set.singleton build_id)


testBotCommentGeneration conn raw_commit@(Builds.RawCommit commit_sha1_text) oauth_access_token = do
  let validated_sha1 = fromRight (error "BAD1") $ GitRev.validateSha1 commit_sha1_text

  blah2 <- flip runReaderT conn $ SqlUpdate.findKnownBuildBreakages
      oauth_access_token
      Constants.pytorchOwnedRepo
      raw_commit

  let upstream_breakages_info = fromRight (error "BAD2") blah2

  blah3 <- liftIO $ flip runReaderT conn $
    StatusUpdate.fetchCommitPageInfo upstream_breakages_info raw_commit validated_sha1


  blah_circleci_failed_job_names <- flip runReaderT conn $ SqlRead.getFailedCircleCIJobNames raw_commit

  let circleci_failed_job_names = fromRight (error "BAD3") blah_circleci_failed_job_names

      build_summary_stats = StatusUpdateTypes.NewBuildSummaryStats
        upstream_breakages_info
        circleci_failed_job_names


      commit_page_info = fromRight (error "BAD4") blah3

      Builds.RawCommit merge_base_commit_text = SqlUpdate.merge_base upstream_breakages_info

  blah4 <- GadgitFetch.getIsAncestor $
      GadgitFetch.RefAncestryProposition merge_base_commit_text StatusUpdate.viableBranchName
  let ancestry_result = fromRight (error "BAD5") blah4

      middle_sections = CommentRender.generateMiddleSections
        ancestry_result
        build_summary_stats
        commit_page_info
        raw_commit

  putStrLn $ T.unpack $
--    T.unlines $ CommentRender.genBuildFailuresTable commit_page_info build_summary_stats
    CommentRender.generateCommentMarkdown
      Nothing
      middle_sections
      raw_commit


testGetSpanningBreakages conn args =
  runExceptT $ do
    rsa_signer <- except $ CircleAuth.loadRsaKey $ gitHubAppPemContent args
    let third_party_auth = CircleApi.ThirdPartyAuth
          (CircleApi.CircleCIApiToken $ T.pack $ circleciApiToken args)
          rsa_signer
          (AmazonQueueData.QueueURL $ sqsQueueUrl args)

    scan_resources <- ExceptT $ Scanning.prepareScanResources
      third_party_auth
      conn
      Scanning.NoPersist
      Nothing

    foo <- ExceptT $ SqlRead.getSpanningBreakages
      conn
      (Builds.RawCommit "c1fa71972e9cdc50562d6f807fc6ca893c585112")

    liftIO $ do
      D.debugList [
          "Results length:"
        , show $ length foo
        ]

      for_ foo $ \x ->
        D.debugList [
            "Result:"
          , show x
          ]


testWithAccessToken args f =
  runExceptT $ do
    rsa_signer <- except $ CircleAuth.loadRsaKey $ gitHubAppPemContent args
    let third_party_auth = CircleApi.ThirdPartyAuth
          (CircleApi.CircleCIApiToken $ T.pack $ circleciApiToken args)
          rsa_signer
          (AmazonQueueData.QueueURL $ sqsQueueUrl args)

    access_token_container <- ExceptT $ fmap (first T.pack) $
      CircleAuth.getGitHubAppInstallationToken $ CircleApi.jwt_signer third_party_auth

    let access_token = CircleAuth.token access_token_container

    liftIO $ f access_token


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  hSetBuffering stdout LineBuffering

  conn <- DbPreparation.prepareDatabase connection_data False

--  benchmarkScan circletoken conn $ Builds.UniversalBuildId 82047188

--  testGetSpanningBreakages conn args

  testWithAccessToken args $ testBotCommentGeneration conn $ Builds.RawCommit "e05f97f167439d3133494c3fa6538b00172a5565"


  {-
  AmazonQueue.sendSqsMessage $
    AmazonQueueData.SqsBuildScanMessage
      (Builds.RawCommit "bce9ad0413a2327ca05d809cf7aaca4fac757457")
      "testing"
  -}

--  GadgitTest.testGadgitApis
--  putStrLn "============================="


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
    circletoken = CircleApi.CircleCIApiToken $ T.pack $ circleciApiToken args
    git_repo_dir = repoGitDir args

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

