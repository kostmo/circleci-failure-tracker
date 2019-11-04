{-# LANGUAGE OverloadedStrings #-}

module Markdown where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree (Forest)
import qualified Data.Tree as Tree


import           Data.List (intersperse)


italic :: Text -> Text
italic x = mconcat ["*", x, "*"]


parens :: Text -> Text
parens x = "(" <> x <> ")"


heading :: Int -> Text -> Text
heading level title = T.unwords [
    mconcat $ replicate level "#"
  , title
  ]


bracket :: Text -> Text
bracket x = "[" <> x <> "]"


link :: Text -> Text -> Text
link label url = bracket label <> parens url


bulletize depth content = indentation <> "* " <> content
  where
    indentation = mconcat $ replicate (4 * depth) " "


bullets :: [Text] -> Text
bullets = T.unlines . map (bulletize 0)


-- | Adds a period at the end of a list of words.
sentence ::  [Text] -> Text
sentence pieces = T.unwords pieces <> "."


-- | Inserts blank lines between each element
paragraphs :: [Text] -> Text
paragraphs = T.unlines . intersperse ""



bulletTree :: Forest Text -> Text
bulletTree = T.unlines . map (uncurry bulletize) . bulletTreeRecurse 0


bulletTreeRecurse :: Int -> Forest Text -> [(Int, Text)]
bulletTreeRecurse depth = mconcat . map go
  where
    go t = (depth, Tree.rootLabel t) : bulletTreeRecurse (depth + 1) (Tree.subForest t)

