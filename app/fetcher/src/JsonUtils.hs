module JsonUtils where

import           Data.Aeson
import           Data.Text  (Text)


dropUnderscore = defaultOptions {fieldLabelModifier = drop 1}


class WithErrorMessage a where
  getMessage :: a -> Text


instance WithErrorMessage Text where
  getMessage = id
