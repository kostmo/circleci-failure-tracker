{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DbPreparation where

import           Control.Monad              (when)
import           Data.Foldable              (for_)
import           Database.PostgreSQL.Simple

import qualified Constants
import qualified DbHelpers
import qualified PatternsFetch
import qualified SqlWrite


buildDataTruncations :: [Query]
buildDataTruncations = [
    "TRUNCATE log_metadata CASCADE;"
  , "TRUNCATE broken_build_reports CASCADE;"
  , "TRUNCATE build_steps CASCADE;"
  , "TRUNCATE created_github_statuses CASCADE;"
  , "TRUNCATE presumed_stable_branches CASCADE;"
  , "TRUNCATE builds CASCADE;"
  , "TRUNCATE ordered_master_commits CASCADE;"
  ]


scanTruncations :: [Query]
scanTruncations = [
    "TRUNCATE scanned_patterns CASCADE;"
  , "TRUNCATE scans CASCADE;"
  , "TRUNCATE matches CASCADE;"
  , "TRUNCATE pattern_step_applicability CASCADE;"
  , "TRUNCATE pattern_tags CASCADE;"
  , "TRUNCATE pattern_authorship CASCADE;"
  , "TRUNCATE patterns CASCADE;"
  ]


-- | We do not wipe the "builds" or "build_steps" tables
-- because visiting each build is expensive.
allTableTruncations :: [Query]
allTableTruncations = scanTruncations ++ buildDataTruncations


prepare_database :: DbHelpers.DbConnectionData -> Bool -> IO Connection
prepare_database conn_data wipe = do

  conn <- DbHelpers.get_connection conn_data

  when wipe $ do
    scrub_tables conn
    PatternsFetch.populate_patterns conn_data
    SqlWrite.populate_presumed_stable_branches conn Constants.presumedGoodBranches
    return ()
  return conn


scrub_tables :: Connection -> IO ()
scrub_tables conn = do

  for_ table_truncation_commands $ \table_truncation_command -> do
    execute_ conn table_truncation_command

  where
--    table_truncation_commands = allTableTruncations
    table_truncation_commands = scanTruncations
