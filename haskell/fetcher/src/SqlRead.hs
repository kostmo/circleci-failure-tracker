{-# LANGUAGE OverloadedStrings #-}

module SqlRead where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (forM)
import Database.PostgreSQL.Simple
import Data.Time.Format (parseTimeOrError, defaultTimeLocale, rfc822DateFormat)

import Builds


get_connection :: IO Connection
get_connection = connect $ defaultConnectInfo {
      connectUser = "logan"
    , connectPassword = "logan01"
    , connectDatabase = "loganci"
    }


hello :: IO Int
hello = do
  conn <- get_connection
  [Only i] <- query_ conn "select 2 + 2"
  return i


query_builds :: IO [Build]
query_builds = do
  conn <- get_connection

  xs <- query_ conn "SELECT build_num, vcs_revision, queued_at, job_name FROM builds"
  forM (xs :: [(Int, Text, Text, Text)]) $ \(buildnum, vcs_rev, queuedat_string, jobname) -> let
      queuedat = parseTimeOrError False defaultTimeLocale rfc822DateFormat $ T.unpack queuedat_string
    in return $ NewBuild (NewBuildNumber buildnum) vcs_rev queuedat jobname

        


