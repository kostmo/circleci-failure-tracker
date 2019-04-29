{-# LANGUAGE OverloadedStrings #-}

module BuildRetrieval where

import           Control.Lens               hiding ((<.>))
import           Data.Aeson                 (Value, decode, encode)
import           Data.Aeson.Lens            (key, _Array, _Integral, _String)


import           Data.Fixed                 (Fixed (MkFixed))
import           Data.List                  (intercalate)
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import qualified Data.Time.Clock            as Clock
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess
import qualified Safe

import           Builds
import qualified Constants
import qualified FetchHelpers
import           SillyMonoids               ()
import qualified SqlWrite


maxBuildPerPage :: Int
maxBuildPerPage = 100


updateBuildsList :: Connection -> Int -> Int -> IO Int64
updateBuildsList conn fetch_count age_days = do
  putStrLn "Fetching builds list..."
  downloaded_builds_list <- populate_builds fetch_count age_days

  putStrLn "Storing builds list..."
  SqlWrite.store_builds_list conn downloaded_builds_list


itemToBuild :: Value -> Build
itemToBuild json = NewBuild {
    build_id = NewBuildNumber $ view (key "build_num" . _Integral) json
  , vcs_revision = view (key "vcs_revision" . _String) json
  , queued_at = head $ Maybe.fromJust $ decode (encode [queued_at_string])
  , job_name = view (key "workflows" . key "job_name" . _String) json
  , branch = view (key "branch" . _String) json
  }
  where
    queued_at_string = view (key "queued_at" . _String) json


get_build_list_url :: String -> String
get_build_list_url branch_name = intercalate "/"
  [ Constants.circleci_api_base
  , "tree"
  , branch_name
  ]


populate_builds :: Int -> Int -> IO [Build]
populate_builds max_build_count max_age_days = do

  sess <- Sess.newSession
  current_time <- Clock.getCurrentTime
  let seconds_per_day = Clock.nominalDiffTimeToSeconds $ Clock.nominalDay
      seconds_offset = seconds_per_day * (MkFixed $ fromIntegral max_age_days)
      time_diff = Clock.secondsToNominalDiffTime seconds_offset
      earliest_requested_time = Clock.addUTCTime time_diff current_time
  populate_builds_recurse sess 0 earliest_requested_time max_build_count


populate_builds_recurse :: Sess.Session -> Int -> UTCTime -> Int -> IO [Build]
populate_builds_recurse sess offset earliest_requested_time max_build_count = do

  if max_build_count > 0
    then do

      putStrLn $ unwords [
          "Getting builds starting at"
        , show offset
        , "(" ++ show max_build_count ++ " left)"
        ]

      builds <- get_single_build_list sess builds_per_page offset

      let fetched_build_count = length builds
          builds_left = max_build_count - fetched_build_count

      case Safe.minimumMay $ map Builds.queued_at builds of
        Nothing ->
          putStrLn "No builds found."
        Just earliest_build_time ->
          putStrLn $ "Earliest build time found: " ++ show earliest_build_time

      let next_offset = offset + fetched_build_count
      more_builds <- populate_builds_recurse sess next_offset earliest_requested_time builds_left
      return $ builds ++ more_builds

  else
    return []

  where
    builds_per_page = min maxBuildPerPage max_build_count


-- TODO - these are methods of parallelization:
--  pages <- withTaskGroup 4 $ \g -> mapConcurrently g Scanning.store_log scannable
--  pages <- mapConcurrently Scanning.store_log scannable
--  pages <- withPool 1 $ \pool -> parallel_ pool $ map Scanning.store_log scannable


get_single_build_list :: Sess.Session -> Int -> Int -> IO [Build]
get_single_build_list sess limit offset = do

  either_r <- FetchHelpers.safeGetUrl $ Sess.getWith opts sess fetch_url

  case either_r of
    Right r -> do

      let inner_list = r ^. NW.responseBody . _Array
          builds_list = map itemToBuild $ V.toList inner_list

      return builds_list
    Left err_message -> do
      putStrLn $ "PROBLEM: Failed in get_single_build_list with message: " ++ err_message
      return []

  where
    fetch_url = get_build_list_url "master"
    opts = defaults
      & header "Accept" .~ [Constants.json_mime_type]
      & param "shallow" .~ ["true"]
      & param "filter" .~ ["failed"]
      & param "offset" .~ [T.pack $ show offset]
      & param "limit" .~ [T.pack $ show limit]

