{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Either                (fromRight)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Network.OAuth.OAuth2       as OAuth2
import           Options.Applicative
import           System.IO

import qualified Builds
import qualified CommentRender
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
import qualified StatusUpdateTypes


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



  let commit_sha1_text = "143e1e60f0486862c9d5636b62ac5fa7ff7653e0"
      raw_commit = Builds.RawCommit commit_sha1_text
      validated_sha1 = fromRight (error "BAD") $ GitRev.validateSha1 commit_sha1_text

  blah <- runReaderT (SqlRead.getRevisionBuilds validated_sha1) conn
  let DbHelpers.BenchmarkedResponse _ revision_builds = fromRight (error "BAD2") blah

  blah2 <- SqlUpdate.findKnownBuildBreakages
      conn
      oauth_access_token
      Constants.pytorchOwnedRepo
      raw_commit

  let upstream_breakages_info = fromRight (error "BAD3") blah2

  blah3 <- liftIO $
    runReaderT (StatusUpdate.fetchCommitPageInfo upstream_breakages_info raw_commit validated_sha1) conn

  let build_summary_stats = StatusUpdateTypes.NewBuildSummaryStats
        3
        upstream_breakages_info
        []



  let commit_page_info = fromRight (error "BAD4") blah3


  let Builds.RawCommit merge_base_commit_text = SqlUpdate.merge_base upstream_breakages_info

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

