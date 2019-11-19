{-# LANGUAGE OverloadedStrings #-}

module MyUtils where

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


applyIf :: Bool -> (a -> a) -> a -> a
applyIf should func = f
  where
  f = if should then func else id


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


timeThisFloat :: MonadIO m => m a -> m (Float, a)
timeThisFloat ioa = do
  (t1, t2, a) <- timeThisT ioa
  return (fromIntegral (toNanoSecs $ diffTimeSpec t2 t1) / fromIntegral (10 ^ 9), a)


timeThisT :: MonadIO m => m a -> m (TimeSpec, TimeSpec, a)
timeThisT ioa = do
  t1 <- liftIO $ getTime Monotonic
  a <- ioa
  t2 <- liftIO $ getTime Monotonic
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
