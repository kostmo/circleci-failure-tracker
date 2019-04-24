{-# LANGUAGE OverloadedStrings #-}

import Data.Either (lefts)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import           Options.Applicative

import Control.Concurrent.ParallelIO.Local (withPool, parallel_)

import Control.Concurrent (getNumCapabilities)

import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlWrite
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


  SqlWrite.scrub_tables

  capability_count <- getNumCapabilities
  print $ "Num capabilities: " ++ show capability_count

  builds_list <- Scanning.populate_builds fetch_count

  SqlWrite.store_builds_list builds_list

  failure_info_eithers <- Scanning.get_all_failed_build_info builds_list

  let failure_infos = lefts failure_info_eithers
      scannable = Maybe.mapMaybe (Scanning.filter_scannable) failure_infos

  Scanning.store_all_logs scannable

  matches <- mapM (Scanning.scan_logs ScanPatterns.pattern_list . fst) scannable
  print matches

  db_answer <- SqlRead.hello
  print db_answer

  build_list <- SqlRead.query_builds
  print $ "Build count: " ++ show (length build_list)

  where
    title_string = title args
    fetch_count = buildCount args


main :: IO ()
main = execParser opts >>= mainAppCode
  where
    opts = info (helper <*> myCliParser)
      ( fullDesc
     <> progDesc "Scans CircleCI failure logs"
     <> header "fetcher - performs the scan, populates database" )

