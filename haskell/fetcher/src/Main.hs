{-# LANGUAGE OverloadedStrings #-}

import Data.Either (lefts)
import qualified Data.Maybe as Maybe
import Data.Traversable (for)

import Database.PostgreSQL.Simple


import qualified Scanning
import qualified ScanPatterns




hello :: IO Int
hello = do
  conn <- connect $ defaultConnectInfo {
      connectUser = "logan"
    , connectPassword = "logan01"
    , connectDatabase = "loganci"
    }
  [Only i] <- query_ conn "select 2 + 2"
  return i



main = do

  builds_list <- Scanning.populate_builds 2 3
  failure_info_eithers <- for builds_list Scanning.get_failed_build_info

  let failure_infos = lefts failure_info_eithers
      scannable = Maybe.mapMaybe (Scanning.filter_scannable) failure_infos

  matches <- mapM (Scanning.scan_logs ScanPatterns.pattern_list) scannable
  print matches

  db_answer <- hello
  print db_answer 

