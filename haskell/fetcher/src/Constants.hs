{-# LANGUAGE OverloadedStrings #-}

module Constants where

import           Data.ByteString (ByteString)
import           Data.List       (intercalate)

url_cache_basedir :: String
url_cache_basedir = "/tmp/circleci-download-cache"


json_mime_type :: ByteString
json_mime_type = "application/json"


project_name :: String
project_name = "pytorch"

repo_name :: String
repo_name = "pytorch"


circleci_api_base :: String
circleci_api_base = intercalate "/"
  [ "https://circleci.com/api/v1.1/project/github"
  , project_name
  , repo_name
  ]


