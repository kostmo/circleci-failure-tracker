{-# LANGUAGE OverloadedStrings #-}

module QueryUtils where

import           Control.Monad              (join, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)

import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (intersperse)
import           Database.PostgreSQL.Simple (Query)
import           Formatting
import           Formatting.Clock
import           System.Clock

import qualified Constants


-- | Join SQL queries with interspersed spaces
qjoin :: [Query] -> Query
qjoin = mconcat . intersperse " "


-- | Comma-separated entities
qlist :: [Query] -> Query
qlist = mconcat . intersperse ", "


-- | AND-separated entities
qconjunction :: [Query] -> Query
qconjunction = qjoin . intersperse "AND"


qparens :: Query -> Query
qparens x = mconcat ["(", x, ")"]


-- | Counts the number of fields to ensure
-- the correct number of question marks
insertionValues :: [Query] -> Query
insertionValues fields = MyUtils.qjoin [
    MyUtils.qparens $ MyUtils.qlist fields
  , "VALUES"
  , MyUtils.qparens $ MyUtils.qlist $ replicate (length fields) "?"
  ]
