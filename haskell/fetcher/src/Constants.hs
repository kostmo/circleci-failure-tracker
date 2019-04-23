{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.List (intercalate)
import Data.ByteString (ByteString)


json_mime_type = "application/json" :: ByteString


project_name = "pytorch"
repo_name = "pytorch"


circleci_api_base = intercalate "/"
  [ "https://circleci.com/api/v1.1/project/github"
  , project_name
  , repo_name
  ]


