{-# LANGUAGE OverloadedStrings #-}

module CommentRenderCommon where

import           Data.Text      (Text)
import qualified Data.Text.Lazy as LT
import           GHC.Int        (Int64)

viableBranchName :: Text
viableBranchName = "viable/strict"


webserverBaseUrl :: LT.Text
webserverBaseUrl = "https://dr.pytorch.org"


viableCommitsHistoryUrl :: LT.Text
viableCommitsHistoryUrl = webserverBaseUrl <> "/master-viable-commits.html"


newtype CommentRevisionId = CommentRevisionId Int64


data PrCommentPayload = NewPrCommentPayload {
    sections                       :: [[Text]]
  , all_no_fault_failures          :: Bool
  , all_successful_circleci_builds :: Bool
  }
