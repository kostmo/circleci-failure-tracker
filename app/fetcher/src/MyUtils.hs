{-# LANGUAGE OverloadedStrings #-}

module MyUtils where

import           Control.Monad       (join)

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as T


pluralize :: Int -> Text -> Text
pluralize quantity word =
  T.unwords [
      T.pack $ show quantity
    , word <> suffix
    ]
  where
    suffix = if quantity == 1
      then ""
      else "s"


applyIf :: Bool -> (a -> a) -> a -> a
applyIf should func = f
  where
  f = if should then func else id


binTuplesByFirstAsMap :: (Eq a, Hashable a, Foldable z, Applicative t, Monoid (t b)) =>
   z (a, b) -> HashMap a (t b)
binTuplesByFirstAsMap =
  foldr (uncurry (HashMap.insertWith mappend) . fmap pure) HashMap.empty


binTuplesByFirst :: (Eq a, Hashable a) => [(a, b)] -> [(a, [b])]
binTuplesByFirst = HashMap.toList . binTuplesByFirstAsMap


-- | duplicates the argument into both members of the tuple
duple :: a -> (a, a)
duple = join (,)


-- | Given a function and a value, create a pair
-- where the first element is the value, and the
-- second element is the function applied to the value
derivePair :: (a -> b) -> a -> (a, b)
derivePair g = fmap g . duple


quote :: String -> String
quote x = "\"" <> x <> "\""


parens :: String -> String
parens x = "(" <> x <> ")"
