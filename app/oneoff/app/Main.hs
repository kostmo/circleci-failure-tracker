{-# LANGUAGE OverloadedStrings #-}

import           Data.Text            (Text)
import qualified Network.OAuth.OAuth2 as OAuth2
import           Options.Applicative
import           System.IO

import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DbPreparation
import qualified GadgitFetch
import qualified MyUtils
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



  result1 <- GadgitFetch.getSinglePullRequestHeadCommit $ Builds.PullRequestNumber 27445
  MyUtils.debugList [
      "result1:"
    , show result1
    ]


  result2 <- GadgitFetch.getSinglePullRequestHeadCommit $ Builds.PullRequestNumber 27445547
  MyUtils.debugList [
      "result2:"
    , show result2
    ]


  -- | TODO move these to unit tests
  result3 <- GadgitFetch.getPullRequestHeadCommitsBulk $ map Builds.PullRequestNumber [22201, 23463]
  MyUtils.debugList [
      "result3:"
    , show result3
    ]


  putStrLn "============================="

  conn <- DbPreparation.prepareDatabase connection_data False


  batch_diagnosis_result <- SqlUpdate.diagnoseCommitsBatch
    (Just git_repo_dir)
    conn
    oauth_access_token
    owned_repo

  putStrLn $ unwords [
      "Batch diagnosis result:"
    , show batch_diagnosis_result
    ]



  putStrLn $ unwords [
      "Populating merge base head commits..."
    ]

  SqlWrite.getAllPullRequestHeadCommits conn git_repo_dir


  -- ===========================================
  {-
  pr_head_commits <- runReaderT SqlRead.getAllMergedPullRequestHeadCommits conn
  stuff <- for (zip [1..] pr_head_commits) $ \(idx, head_commit) -> do
    putStrLn $ unwords ["Progress:", show idx, "/", show $ length pr_head_commits, MyUtils.parens $ "HEAD commit " <> show head_commit]
    runExceptT $ StatusUpdate.getBuildsFromGithub conn oauth_access_token owned_repo head_commit
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

