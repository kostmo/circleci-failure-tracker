{-# LANGUAGE OverloadedStrings #-}

module QueryUtils where

import           Data.List                  (intersperse)
import           Database.PostgreSQL.Simple (Query)


-- | Join SQL queries with interspersed spaces
qjoin :: [Query] -> Query
qjoin = mconcat . intersperse " "


-- | Comma-separated entities
list :: [Query] -> Query
list = mconcat . intersperse ", "


-- | AND-separated entities
qconjunction :: [Query] -> Query
qconjunction = qjoin . intersperse "AND"


parens :: Query -> Query
parens = mconcat . (`intersperse` ["(", ")"])


-- | Counts the number of fields to ensure
-- the correct number of question marks
insertionValues :: [Query] -> Query
insertionValues fields = qjoin [
    parens $ list fields
  , "VALUES"
  , parens $ list $ replicate (length fields) "?"
  ]
