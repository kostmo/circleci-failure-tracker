{-# LANGUAGE OverloadedStrings #-}

module Sql.QueryUtils where

import           Data.List                  (intersperse)
import           Database.PostgreSQL.Simple (Query)


-- | Concatenate SQL queries with interspersed spaces
qjoin :: [Query] -> Query
qjoin = mconcat . intersperse " "


-- | Comma-separated entities
list :: [Query] -> Query
list = mconcat . intersperse ", "


-- | AND-separated entities
qconjunction :: [Query] -> Query
qconjunction = qjoin . intersperse "AND"


-- | Surround with parentheses
parens :: Query -> Query
parens = mconcat . (`intersperse` ["(", ")"])


coalesce  :: Query -> Query -> Query -> Query
coalesce from_expr to_expr varname = qjoin [
    "COALESCE" <> parens (list [from_expr, to_expr])
  , "AS"
  , varname
  ]


aliasedSubquery :: Query -> Query -> Query
aliasedSubquery subquery alias = qjoin [
    parens subquery
  , alias
  ]


-- | Ensures that the number of question marks
-- is equal to the number of fields
insertionValues :: [Query] -> Query
insertionValues fields = qjoin [
    parens $ list fields
  , "VALUES"
  , parens $ list $ replicate (length fields) "?"
  ]
