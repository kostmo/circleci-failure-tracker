{-# LANGUAGE OverloadedStrings #-}

module ScanPatterns where

import Data.Text (Text)


pattern_list :: [Text]
pattern_list = [
    "Waiting for a VM assignment"
  , "undefined symbol:"
  ]

