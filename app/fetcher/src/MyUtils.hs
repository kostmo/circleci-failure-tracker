{-# LANGUAGE OverloadedStrings #-}

module MyUtils where

import           Control.Monad       (join, when)

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Formatting
import           Formatting.Clock
import           System.Clock

import qualified Constants


debugList :: [String] -> IO ()
debugList = debugStr . unwords


debugStr :: String -> IO ()
debugStr = when Constants.printDebug . putStrLn


-- | Wrap an 'IO' computation so that it prints out the execution time.
--
-- Borrowed from the "timeit" package here:
-- http://hackage.haskell.org/package/timeit-1.0.0.0/docs/System-TimeIt.html
--
-- See rationale for adapting this function here:
-- https://chrisdone.com/posts/measuring-duration-in-haskell/#accurate-measuring
--
-- The timeit package does not consider wall clock time, only CPU time.
timeThis :: IO a -> IO a
timeThis ioa = do
  (t1, t2, a) <- timeThisT ioa
  fprint (timeSpecs % "\n") t1 t2
  return a


timeThisT :: IO a -> IO (TimeSpec, TimeSpec, a)
timeThisT ioa = do
  t1 <- getTime Monotonic
  a <- ioa
  t2 <- getTime Monotonic
  return (t1, t2, a)


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
