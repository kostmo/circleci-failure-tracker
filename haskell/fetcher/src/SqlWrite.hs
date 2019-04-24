{-# LANGUAGE OverloadedStrings #-}

module SqlWrite where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (forM)
import Data.Foldable (for_)
import Database.PostgreSQL.Simple
import Builds
import qualified Data.ByteString.Char8 as BSC
import Data.Time.Format (formatTime, defaultTimeLocale, rfc822DateFormat)
import GHC.Int (Int64)

import qualified SqlRead


allTableTruncations = [
     "TRUNCATE scanned_patterns CASCADE;"
   , "TRUNCATE scans CASCADE;"
   , "TRUNCATE matches CASCADE;"
   , "TRUNCATE pattern_step_applicability CASCADE;"
   , "TRUNCATE build_steps CASCADE;"
   , "TRUNCATE pattern_tags CASCADE;"
   , "TRUNCATE patterns CASCADE;"
   , "TRUNCATE builds CASCADE;"
  ]


scrub_tables = do

  conn <- SqlRead.get_connection

-- TODO optionally exclude the last table, which is the "builds" table
--table_list = TABLE_NAMES[:-int(preserve_builds)]

  for_ allTableTruncations $ \table_truncation_command -> do
    execute_ conn table_truncation_command


build_to_tuple :: Build -> (Int, Text, Text, Text)
build_to_tuple (NewBuild (NewBuildNumber build_num) vcs_rev queued_at jobname) = (build_num, vcs_rev, queued_at_string, jobname)
  where
    queued_at_string = T.pack $ formatTime defaultTimeLocale rfc822DateFormat queued_at


store_builds_list :: [Build] -> IO Int64
store_builds_list builds_list = do

  conn <- SqlRead.get_connection

  executeMany conn "INSERT INTO builds(build_num, vcs_revision, queued_at, job_name) VALUES(?,?,?,?) ON CONFLICT (build_num) DO NOTHING" $
    map build_to_tuple builds_list

