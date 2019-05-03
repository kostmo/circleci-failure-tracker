{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class        (liftIO)
import           Data.List                     (filter)
import           Data.List.Split               (splitOn)
import qualified Data.Maybe                    as Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Text.Internal.Lazy       as LT
import           Network.Wai.Middleware.Static
import           Options.Applicative
import           System.Directory
import           System.Environment            (lookupEnv)
import           System.FilePath
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S
import qualified Web.Scotty.Internal.Types     as ScottyTypes

import qualified Builds
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
import qualified WebApi


pattern_from_parms :: ScottyTypes.ActionT LT.Text IO ScanPatterns.Pattern
pattern_from_parms = do

  expression <- S.param "pattern"
  is_regex_str <- S.param "is_regex"
  description <- S.param "description"
  tags <- S.param "tags"
  applicable_steps <- S.param "applicable_steps"

  let is_regex = is_regex_str == ("true" :: Text)
      match_expression = if is_regex
            then ScanPatterns.RegularExpression $ encodeUtf8 expression
            else ScanPatterns.LiteralExpression expression

  return $ ScanPatterns.NewPattern
    match_expression
    description
    (listify tags)
    (listify applicable_steps)
  where
    listify = filter (not . T.null) . map (T.strip . T.pack) . splitOn ","


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe 3000 $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  S.scotty prt $ do

    S.middleware $ staticPolicy (noDots >-> addBase static_base)

    S.get "/api/failed-commits-by-day" $
      S.json =<< liftIO SqlRead.api_failed_commits_by_day

    S.get "/api/job" $
      S.json =<< liftIO SqlRead.api_jobs

    S.get "/api/log-size-histogram" $
      S.json =<< liftIO SqlRead.api_line_count_histogram

    S.get "/api/step" $
      S.json =<< liftIO SqlRead.api_step

    S.get "/api/tags" $ do
      term <- S.param "term"
      x <- liftIO $ SqlRead.api_list_tags term
      S.json x

    S.get "/api/new-pattern-test" $ do
      buildnum_str <- S.param "build_num"
      new_pattern <- pattern_from_parms
      x <- liftIO $ SqlWrite.api_new_pattern_test (Builds.NewBuildNumber $ read buildnum_str) new_pattern
      S.json x

    S.post "/api/new-pattern-insert" $ do
      new_pattern <- pattern_from_parms
      x <- liftIO $ SqlWrite.api_new_pattern new_pattern
      S.json x

    S.get "/api/steps" $ do
      term <- S.param "term"
      x <- liftIO $ SqlRead.api_list_steps term
      S.json x

    S.get "/api/random-scannable-build" $
      S.json =<< liftIO SqlRead.api_random_scannable_build

    S.get "/api/summary" $
      S.json =<< liftIO SqlRead.api_summary_stats

    S.get "/api/unmatched-builds" $
      S.json =<< liftIO SqlRead.api_unmatched_builds

    S.get "/api/idiopathic-failed-builds" $
      S.json =<< liftIO SqlRead.api_idiopathic_builds

    S.get "/api/disk" $ do
      S.json =<< liftIO WebApi.api_disk_space

    S.get "/api/pattern" $ do
      pattern_id <- S.param "pattern_id"
      x <- liftIO $ SqlRead.api_single_pattern $ read pattern_id
      S.json x

    S.get "/api/patterns" $ do
      S.json =<< liftIO SqlRead.api_patterns

    S.get "/api/pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      x <- liftIO $ SqlRead.get_pattern_matches pattern_id
      S.json x

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file $ static_base </> "images/favicon.ico"

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do

      liftIO $ do
        cwd <- getCurrentDirectory
        putStrLn $ "Current working dir: " ++ cwd
        dircontents <- getDirectoryContents cwd
        putStrLn $ "Directory contents: " ++ show dircontents
        putStrLn $ "Directory contents of \"" ++ static_base ++ "\":"
        dircontents2 <- getDirectoryContents static_base
        putStrLn $ "-> " ++ show dircontents2

      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file $ static_base </> "index.html"

  where
    static_base = staticBase args


data CommandLineArgs = NewCommandLineArgs {
    serverPort :: Int
  , staticBase :: String
  , dbHostname :: String
  }


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "port"       <> value 3000           <> metavar "PORT"
    <> help "Webserver port")
  <*> strOption   (long "data-path" <> value "/data/static" <> metavar "STATIC_DATA"
    <> help "Path to static data files")
  <*> strOption   (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"
    <> help "Hostname of database")
--  <*> switch      (long "wipe"
--    <> help "Wipe database content before beginning")


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "CircleCI failure log analsys webserver"
     <> header "webapp - user frontend" )


