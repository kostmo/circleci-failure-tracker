{-# LANGUAGE OverloadedStrings #-}

module Markdown where

import           Data.List              (intersperse)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NE
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Data.Tree              (Forest)
import qualified Data.Tree              as Tree
import qualified HTMLEntities.Builder   as HEB

import           Data.Text.Lazy.Builder (toLazyText)


surround :: [Text] -> Text -> Text
surround brackets = mconcat . (`intersperse` brackets)


italic :: Text -> Text
italic = surround ["*", "*"]


bold :: Text -> Text
bold = surround ["**", "**"]


codeInline :: Text -> Text
codeInline = surround ["`", "`"]


sup :: Text -> Text
sup = surround ["<sup>", "</sup>"]


parens :: Text -> Text
parens = surround ["(", ")"]


bracket :: Text -> Text
bracket = surround ["[", "]"]


supTitle :: Text -> Text -> Text
supTitle title = surround brackets
  where
    brackets = [
        "<sup title=\"" <> escaped_title <> "\">"
      , "</sup>"
      ]
    escaped_title = LT.toStrict $ toLazyText $ HEB.text title


heading :: Int -> Text -> Text
heading level title = T.unwords [
    mconcat $ replicate level "#"
  , title
  ]


link :: Text -> Text -> Text
link label url = bracket label <> parens url


image :: Text -> Text -> Text
image tooltip url = "!" <> link tooltip url


delimitColumns :: [Text] -> Text
delimitColumns cols = T.concat padded_cols
  where
    cols_temp = ["|"] <> intersperse "|" cols <> ["|"]
    padded_cols = intersperse " " cols_temp


table :: [Text] -> [[Text]] -> NonEmpty Text
table header_cols data_rows = NE.map delimitColumns all_table_rows
  where
    header_line = replicate (length header_cols) "---"
    all_table_rows = header_cols :| header_line : data_rows


bulletize :: Int -> NonEmpty Text -> NonEmpty Text
bulletize depth (x :| xs) =
  (first_line_indentation <> "* " <> x) :| map (content_indentation <>) xs
  where
    first_line_depth = 4 * depth
    first_line_indentation = mconcat $ replicate first_line_depth " "

    content_depth = first_line_depth + 2
    content_indentation = mconcat $ replicate content_depth " "


codeBlock :: NonEmpty Text -> NonEmpty Text
codeBlock code_lines = pure "```" <> code_lines <> pure "```"


-- | Adds a period at the end of a list of words.
sentence ::  [Text] -> Text
sentence = (<> ".") . T.unwords


-- | Adds a colon at the end of a list of words.
colonize ::  [Text] -> Text
colonize = (<> ":") . T.unwords


-- | Inserts blank lines between each element
paragraphs :: [Text] -> Text
paragraphs = T.unlines . intersperse ""


bulletTree :: Forest (NonEmpty Text) -> Text
bulletTree = T.unlines . concatMap (NE.toList . uncurry bulletize) . flattenWithDepth 0


-- | Typically should call this with 0 as the first argument
flattenWithDepth :: Int -> Forest a -> [(Int, a)]
flattenWithDepth depth = concatMap go
  where
    go t = (depth, Tree.rootLabel t) : flattenWithDepth (depth + 1) (Tree.subForest t)
