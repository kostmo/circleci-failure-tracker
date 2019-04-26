{-# LANGUAGE OverloadedStrings #-}

module SqlWrite where

import           Builds
import           Data.Foldable              (for_)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             rfc822DateFormat)
import           Database.PostgreSQL.Simple
import           GHC.Int                    (Int64)

import qualified ScanPatterns


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


scrub_tables :: Connection -> IO ()
scrub_tables conn = do

  for_ table_truncation_commands $ \table_truncation_command -> do
    execute_ conn table_truncation_command

  where
    table_truncation_commands = allTableTruncations

    -- TODO optionally exclude the last table, which is the "builds" table
    -- table_truncation_commands = init allTableTruncations


build_to_tuple :: Build -> (Int64, Text, Text, Text)
build_to_tuple (NewBuild (NewBuildNumber build_num) vcs_rev queued_at jobname) = (build_num, vcs_rev, queued_at_string, jobname)
  where
    queued_at_string = T.pack $ formatTime defaultTimeLocale rfc822DateFormat queued_at


store_builds_list :: Connection -> [Build] -> IO Int64
store_builds_list conn builds_list = do

  executeMany conn "INSERT INTO builds(build_num, vcs_revision, queued_at, job_name) VALUES(?,?,?,?) ON CONFLICT (build_num) DO NOTHING" $
    map build_to_tuple builds_list


populate_patterns :: Connection -> [ScanPatterns.Pattern] -> IO ()
populate_patterns conn pattern_list = do

  for_ pattern_list $ \(ScanPatterns.NewPattern is_regex pattern_text description tags applicable_steps) -> do

    [Only pattern_id] <- query conn pattern_insertion_sql (is_regex, pattern_text, description, False, False)

    for_ tags $ \tag -> do
      execute conn tag_insertion_sql (tag, pattern_id :: Int)

    for_ applicable_steps $ \applicable_step ->
      execute conn applicable_step_insertion_sql (applicable_step, pattern_id)

  where
    pattern_insertion_sql = "INSERT INTO patterns(regex, expression, description, is_infra, has_nondeterministic_values) VALUES(?,?,?,?,?) RETURNING id;"
    tag_insertion_sql = "INSERT INTO pattern_tags(tag, pattern) VALUES(?,?);"
    applicable_step_insertion_sql = "INSERT INTO pattern_step_applicability(step_name, pattern) VALUES(?,?);"


step_failure_to_tuple :: (BuildNumber, Maybe BuildStepFailure) -> (Int64, Maybe Text, Bool)
step_failure_to_tuple (NewBuildNumber build_id, maybe_thing) = case maybe_thing of
  Nothing -> (build_id, Nothing, False)
  Just (NewBuildStepFailure step_name mode) -> let
    is_timeout = case mode of
      BuildTimeoutFailure              -> True
      ScannableFailure _failure_output -> False
     in (build_id, Just step_name, is_timeout)


insert_build_visitations :: Connection -> [(BuildNumber, Maybe BuildStepFailure)] -> IO Int64
insert_build_visitations conn visitations = do

  executeMany conn "INSERT INTO build_steps(build, name, is_timeout) VALUES(?,?,?)" $
    map step_failure_to_tuple visitations


insert_scan_id :: Connection -> IO Int64
insert_scan_id conn = do
  [Only pattern_id] <- query_ conn "INSERT INTO scans(timestamp) VALUES(DEFAULT) RETURNING id;"
  return pattern_id

