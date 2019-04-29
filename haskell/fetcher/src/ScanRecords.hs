{-# LANGUAGE OverloadedStrings #-}

module ScanRecords where

import           Data.HashMap.Strict        (HashMap)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import qualified Network.Wreq.Session       as Sess

import qualified ScanPatterns


data PatternId = NewPatternId Int64


data ScanCatchupResources = ScanCatchupResources {
    db_conn           :: Connection
  , aws_sess          :: Sess.Session
  , circle_sess       :: Sess.Session
  , scan_id           :: Int64
  , newest_pattern_id :: PatternId
  , patterns_by_id    :: HashMap Int64 ScanPatterns.Pattern
  }

