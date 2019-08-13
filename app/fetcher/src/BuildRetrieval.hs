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
import           Data.Traversable           (for)
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import           Network.Wreq               as NW
import qualified Network.Wreq.Session       as Sess
import qualified Safe

import qualified Builds
import qualified Constants
import qualified FetchHelpers
import qualified MyUtils
import           SillyMonoids               ()
import qualified SqlWrite


maxBuildPerPage :: Int
maxBuildPerPage = 100


data CircleCIFetchFilter =
    Completed
  | Failed
  | Successful


toOutcomeString :: CircleCIFetchFilter -> T.Text
toOutcomeString x = case x of
  Completed  -> "completed"
  Failed     -> "failed"
  Successful -> "successful"


-- TODO - these are methods of parallelization:
--  pages <- withTaskGroup 4 $ \g -> mapConcurrently g Scanning.store_log scannable
--  pages <- mapConcurrently Scanning.store_log scannable
--  pages <- withPool 1 $ \pool -> parallel_ pool $ map Scanning.store_log scannable


-- | This is CircleCI-specific
-- and assumes all the builds are failed builds
updateCircleCIBuildsList ::
     Connection
  -> CircleCIFetchFilter
  -> [String]
  -> Int
  -> Int
  -> IO Int64
updateCircleCIBuildsList
    conn
    status_filter
    branch_names
    fetch_count
    age_days = do

  builds_lists <- for branch_names $ \branch_name -> do

    MyUtils.debugList [
        "Fetching builds list for branch"
      , MyUtils.quote branch_name ++ "..."
      ]

    fetchCircleCIBuilds
      status_filter
      branch_name
      fetch_count
      age_days

  let combined_builds_list = concat builds_lists
      succeeded_count = length $ filter snd combined_builds_list
      failed_count = length $ filter (not . snd) combined_builds_list

  MyUtils.debugList [
    "Storing builds list with"
    , show succeeded_count
    , "succeeded and"
    , show failed_count
    , "failed"
    ]

  SqlWrite.storeCircleCiBuildsList conn combined_builds_list


-- | This is populated from the "bulk" API query which lists multiple builds.
-- Contrast with CircleBuild.SingleBuild, for which the API returns
-- one build at a time.
itemToBuild :: Value -> (Builds.Build, Bool)
itemToBuild json = (b, did_succeed)
  where
    b = Builds.NewBuild {
      Builds.build_id = Builds.NewBuildNumber $ view (key "build_num" . _Integral) json
    , Builds.vcs_revision = Builds.RawCommit $ view (key "vcs_revision" . _String) json
    , Builds.queued_at = head $ Maybe.fromJust $ decode (encode [queued_at_string])
    , Builds.job_name = view (key "workflows" . key "job_name" . _String) json
    , Builds.branch = Just $ view (key "branch" . _String) json
    , Builds.start_time = Just $ head $ Maybe.fromJust $ decode (encode [start_time_string])
    , Builds.stop_time = Just $ head $ Maybe.fromJust $ decode (encode [stop_time_string])
    }

    queued_at_string = view (key "queued_at" . _String) json
    start_time_string = view (key "start_time" . _String) json
    stop_time_string = view (key "stop_time" . _String) json
    outcome_string = view (key "outcome" . _String) json

    did_succeed = outcome_string == "success"


getBuildListUrl :: String -> String
getBuildListUrl branch_name = intercalate "/"
  [ Constants.circleci_api_base
  , "tree"
  , branch_name
  ]


fetchCircleCIBuilds ::
     CircleCIFetchFilter
  -> String
  -> Int
  -> Int
  -> IO [(Builds.Build, Bool)]
fetchCircleCIBuilds
    status_filter
    branch_name
    max_build_count
    max_age_days = do

  sess <- Sess.newSession
  current_time <- Clock.getCurrentTime

  let seconds_per_day = Clock.nominalDiffTimeToSeconds Clock.nominalDay
      seconds_offset = seconds_per_day * (MkFixed $ fromIntegral max_age_days)
      time_diff = Clock.secondsToNominalDiffTime seconds_offset
      earliest_requested_time = Clock.addUTCTime time_diff current_time

  fetchCircleCIBuildsRecurse
    sess
    status_filter
    branch_name
    0
    earliest_requested_time
    max_build_count


fetchCircleCIBuildsRecurse ::
     Sess.Session
  -> CircleCIFetchFilter
  -> String
  -> Int
  -> UTCTime
  -> Int
  -> IO [(Builds.Build, Bool)]
fetchCircleCIBuildsRecurse
    sess
    status_filter
    branch_name
    offset
    earliest_requested_time
    max_build_count =

  if max_build_count > 0
    then do

      MyUtils.debugList [
          "Getting builds starting at"
        , show offset
        , MyUtils.parens $ show max_build_count ++ " left"
        ]

      builds <- getSingleBuildList
        sess
        status_filter
        branch_name
        builds_per_page
        offset

      let fetched_build_count = length builds
          builds_left = max_build_count - fetched_build_count

          queued_times = map (Builds.queued_at . fst) builds

      case Safe.minimumMay queued_times of
        Nothing ->
          MyUtils.debugStr "No more builds found."

        -- TODO use this time value
        Just earliest_build_time ->
          MyUtils.debugList [
              "Earliest build time found:"
            , show earliest_build_time
            ]


      let next_offset = offset + fetched_build_count

      -- If the server returned fewer builds than we asked for,
      -- then we know that was the last page of available builds.
      more_builds <- if fetched_build_count < builds_per_page
        then do
          MyUtils.debugList [
            "The earliest build for branch"
            , MyUtils.quote branch_name
            , "has been retrieved."
            ]

          return []

        else fetchCircleCIBuildsRecurse
          sess
          status_filter
          branch_name
          next_offset
          earliest_requested_time
          builds_left

      return $ builds ++ more_builds

  else
    return []

  where
    builds_per_page = min maxBuildPerPage max_build_count


getSingleBuildList ::
     Sess.Session
  -> CircleCIFetchFilter
  -> String
  -> Int
  -> Int
  -> IO [(Builds.Build, Bool)]
getSingleBuildList sess filter_mode branch_name limit offset = do

  either_r <- FetchHelpers.safeGetUrl $ Sess.getWith opts sess fetch_url

  case either_r of
    Right r -> do

      let inner_list = r ^. NW.responseBody . _Array
          builds_list = map itemToBuild $ V.toList inner_list

      return builds_list

    Left err_message -> do
      MyUtils.debugList [
          "PROBLEM: Failed in getSingleBuildList with message:"
        , err_message
        ]

      return []

  where
    fetch_url = getBuildListUrl branch_name
    opts = defaults
      & header "Accept" .~ [Constants.jsonMimeType]
      & param "shallow" .~ ["true"]
      & param "filter" .~ [toOutcomeString filter_mode]
      & param "offset" .~ [T.pack $ show offset]
      & param "limit" .~ [T.pack $ show limit]
