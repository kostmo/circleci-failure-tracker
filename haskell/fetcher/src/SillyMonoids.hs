{-# LANGUAGE OverloadedStrings #-}

-- | These Monoid instances are apparently required
-- to extract primitives from JSON fields.
-- They are arbitrary instances, as we don't
-- actually use the instance methods.
module SillyMonoids where

import           GHC.Int (Int64)


instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0


instance Semigroup Int64 where
  (<>) = (+)

instance Monoid Int64 where
  mempty = 0


instance Semigroup Bool where
  (<>) = (||)

instance Monoid Bool where
  mempty = False

