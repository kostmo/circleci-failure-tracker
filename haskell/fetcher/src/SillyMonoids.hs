{-# LANGUAGE OverloadedStrings #-}

-- | These Monoid instances are apparently required
-- to extract primitives from JSON fields.
module SillyMonoids where


-- XXX This is an arbitrary interpretation as a Monoid
instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0


-- XXX This is an arbitrary interpretation as a Monoid
instance Semigroup Bool where
  (<>) = (||)

instance Monoid Bool where
  mempty = False

