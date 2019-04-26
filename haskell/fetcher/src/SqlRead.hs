{-# LANGUAGE OverloadedStrings #-}

module SqlRead where

import           Control.Monad              (forM)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List.Split            (splitOn)
import qualified Data.Maybe                 as Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Database.PostgreSQL.Simple
import           GHC.Int                    (Int64)

import           Builds
import qualified DbHelpers
import qualified ScanPatterns


get_unscanned_build_patterns :: Connection -> HashMap Int64 ScanPatterns.Pattern -> IO [(BuildNumber, [ScanPatterns.DbPattern])]
get_unscanned_build_patterns conn patterns_by_id = do

  unscanned_patterns_list <- query_ conn sql

  buildnum_patt_id_tuples <- forM unscanned_patterns_list $ \(buildnum, comma_sep_pattern_ids) -> let
        pattern_ids = Set.fromList $ map read $ splitOn "," $ comma_sep_pattern_ids
        patterns = Maybe.mapMaybe (\x -> DbHelpers.WithId x <$> HashMap.lookup x patterns_by_id) $ Set.toList pattern_ids
    in return $ (NewBuildNumber buildnum, patterns)

  return buildnum_patt_id_tuples
  where
    sql = "SELECT build_num, unscanned_patts FROM unscanned_patterns ORDER BY patt_count"


get_patterns :: Connection -> IO [ScanPatterns.DbPattern]
get_patterns conn = do

  patterns_rows <- query_ conn patterns_sql

  forM patterns_rows $ \(pattern_id, is_regex, pattern_text, description) -> do

    tags_rows <- query conn tags_sql (Only pattern_id)
    tags_list <- forM tags_rows $ \(Only tag_text) -> return tag_text

    steps_rows <- query conn applicable_steps_sql (Only pattern_id)
    steps_list <- forM steps_rows $ \(Only step_text) -> return step_text

    let inner_pattern = ScanPatterns.NewPattern is_regex pattern_text description tags_list steps_list
        outer_pattern = DbHelpers.WithId pattern_id inner_pattern

    return outer_pattern

  where
    patterns_sql = "SELECT id, regex, expression, description FROM patterns ORDER BY description;"

    tags_sql = "SELECT tag FROM pattern_tags WHERE pattern = ?;"
    applicable_steps_sql = "SELECT step_name FROM pattern_step_applicability WHERE pattern = ?;"


get_unvisited_build_ids :: Connection -> IO [BuildNumber]
get_unvisited_build_ids conn = do
  rows <- query_ conn sql
  forM rows $ \(Only num) -> return $ NewBuildNumber num
  where
    sql = "SELECT build_num FROM unvisited_builds ORDER BY build_NUM DESC;"


query_builds :: IO [Build]
query_builds = do
  conn <- DbHelpers.get_connection

  xs <- query_ conn "SELECT build_num, vcs_revision, queued_at, job_name FROM builds"
  forM xs $ \(buildnum, vcs_rev, queuedat, jobname) ->
    return $ NewBuild (NewBuildNumber buildnum) vcs_rev queuedat jobname
