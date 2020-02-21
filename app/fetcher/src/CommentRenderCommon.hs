{-# LANGUAGE OverloadedStrings #-}

module CommentRenderCommon where

import           Data.Text      (Text)
import qualified Data.Text.Lazy as LT


viableBranchName :: Text
viableBranchName = "viable/strict"


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://dr.pytorch.org"


viableCommitsHistoryUrl :: LT.Text
viableCommitsHistoryUrl = webserverBaseUrl <> "/master-viable-commits.html"

