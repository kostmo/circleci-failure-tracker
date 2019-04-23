{-# LANGUAGE OverloadedStrings #-}

import Data.Either (lefts)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Traversable (for)
import           Options.Applicative
import System.Directory (createDirectoryIfMissing)

import Control.Concurrent.ParallelIO.Local (withPool, parallel_)
--import Control.Concurrent.Async (mapConcurrently)

import Control.Concurrent (getNumCapabilities)

import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified Constants


data CommandLineArgs = NewCommandLineArgs {
      buildCount :: Int
    , title      :: String
    , quiet      :: Bool
      -- ^ Suppress console output
  } deriving (Show)


myCliParser :: Parser CommandLineArgs
myCliParser = NewCommandLineArgs
  <$> option auto (long "count"     <> value 20          <> metavar "BUILD_COUNT" <> help "How many failed builds to fetch from CircleCI")
  <*> strOption (long "db-hostname" <> value "localhost" <> metavar "DATABASE_HOSTNAME"  <> help "Hostname of database")
  <*> switch    (long "quiet"                                       <> help "Suppress console output")


mainAppCode :: CommandLineArgs -> IO ()
mainAppCode args = do

  capability_count <- getNumCapabilities
  print $ "Num capabilities: " ++ show capability_count

  builds_list <- Scanning.populate_builds fetch_count builds_per_page
  failure_info_eithers <- for builds_list Scanning.get_failed_build_info

  let failure_infos = lefts failure_info_eithers
      scannable = Maybe.mapMaybe (Scanning.filter_scannable) failure_infos

  createDirectoryIfMissing True Constants.url_cache_basedir


--  pages <- withTaskGroup 4 $ \g -> mapConcurrently g Scanning.store_log scannable
--  pages <- mapConcurrently Scanning.store_log scannable

  pages <- withPool 1 $ \pool -> parallel_ pool $ map Scanning.store_log scannable

--  matches <- mapM (Scanning.scan_logs ScanPatterns.pattern_list) scannable
--  print matches

  db_answer <- SqlRead.hello
  print db_answer
  build_list <- SqlRead.query_builds

  print $ "Build count: " ++ show (length build_list)

  where

    builds_per_page = min 100 fetch_count
    title_string = title args
    fetch_count = buildCount args


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Scans CircleCI failure logs"
     <> header "fetcher - performs the scan, populates database" )

