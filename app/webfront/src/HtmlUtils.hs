module HtmlUtils where

import           Data.Foldable    (for_)
import qualified Text.Blaze.Html5 as H hiding (map)


make_table :: [H.Html] -> [[H.Html]] -> H.Html
make_table headings rows = H.table $ H.tbody $ do
  H.tr $ for_ headings H.th
  for_ rows $ \row -> do
    H.tr $ for_ row H.td
