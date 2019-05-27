{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JsonUtils where

import           Data.Aeson
import           Data.Text  (Text)
import qualified Data.Text  as T


dropUnderscore = defaultOptions {fieldLabelModifier = drop 1}


class WithErrorMessage a where
  getMessage :: a -> Text


instance WithErrorMessage Text where
  getMessage = id

instance WithErrorMessage String where
  getMessage = T.pack
