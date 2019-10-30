{-# LANGUAGE OverloadedStrings #-}

module Markdown where

import           Data.Text (Text)
import qualified Data.Text as T

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


bullets :: [Text] -> Text
bullets = T.unlines . map ("* " <>)


-- | Adds a period at the end of a list of words.
sentence ::  [Text] -> Text
sentence pieces = T.unwords pieces <> "."


-- | Inserts blank lines between each element
paragraphs :: [Text] -> Text
paragraphs = T.unlines . intersperse ""
