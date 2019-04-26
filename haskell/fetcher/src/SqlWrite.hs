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

import qualified DbHelpers
import qualified ScanPatterns
import qualified SqlRead


-- | We do not wipe the "builds" or "build_steps" tables
-- because visiting each build is expensive.
allTableTruncations = [
    "TRUNCATE scanned_patterns CASCADE;"
  , "TRUNCATE scans CASCADE;"
  , "TRUNCATE matches CASCADE;"
  , "TRUNCATE pattern_step_applicability CASCADE;"
  , "TRUNCATE pattern_tags CASCADE;"
  , "TRUNCATE patterns CASCADE;"
--  , "TRUNCATE build_steps CASCADE;"
--  , "TRUNCATE builds CASCADE;"
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
store_builds_list conn builds_list =

  executeMany conn "INSERT INTO builds(build_num, vcs_revision, queued_at, job_name) VALUES(?,?,?,?) ON CONFLICT (build_num) DO NOTHING" $
    map build_to_tuple builds_list


store_matches :: Connection -> (SqlRead.ScanScope, [ScanPatterns.ScanMatch]) -> IO Int64
store_matches conn (scope, scoped_matches) =
  executeMany conn insertion_sql $ map to_tuple replicated

  where
    replicated = concatMap (\x -> [(scope, x)])  scoped_matches

    to_tuple (scan_scope, match) = (
        step_id
      , DbHelpers.db_id $ ScanPatterns.scanned_pattern match
      , ScanPatterns.line_number match_deets
      , ScanPatterns.line_text match_deets
      , ScanPatterns.start $ ScanPatterns.span match_deets
      , ScanPatterns.end $ ScanPatterns.span match_deets
      )
      where
        match_deets = ScanPatterns.match_details match
        (NewBuildStepId step_id) = SqlRead.build_step_id scan_scope

    insertion_sql = "INSERT INTO matches(build_step, pattern, line_number, line_text, span_start, span_end) VALUES(?,?,?,?,?,?);"




populate_patterns :: Connection -> [ScanPatterns.Pattern] -> IO ()
populate_patterns conn pattern_list = do

  for_ pattern_list $ \(ScanPatterns.NewPattern expression_obj description tags applicable_steps) -> do


    [Only pattern_id] <- query conn pattern_insertion_sql (ScanPatterns.is_regex expression_obj, ScanPatterns.pattern_text expression_obj, description, False, False)

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

