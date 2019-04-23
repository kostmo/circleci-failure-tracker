{-# LANGUAGE OverloadedStrings #-}

module SqlRead where

import qualified Data.Text as T
import Control.Monad (forM)
import Database.PostgreSQL.Simple
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


query_builds :: IO [SimpleBuild]
query_builds = do
  conn <- get_connection

  xs <- query_ conn "SELECT build_num, job_name FROM builds"
  builds_list <- forM xs $ \(buildnum, jobname) -> do
    return $ NewSimpleBuild buildnum jobname

  return builds_list

