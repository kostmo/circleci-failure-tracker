{-# LANGUAGE OverloadedStrings #-}

module GadgitTest where

import qualified Builds
import qualified DebugUtils  as D
import qualified GadgitFetch


testGadgitApis = do
  result1 <- GadgitFetch.getSinglePullRequestHeadCommit $ Builds.PullRequestNumber 27445
  D.debugList [
      "result1:"
    , show result1
    ]


  result2 <- GadgitFetch.getSinglePullRequestHeadCommit $ Builds.PullRequestNumber 27445547
  D.debugList [
      "result2:"
    , show result2
    ]


  -- | TODO move these to unit tests
  result3 <- GadgitFetch.getPullRequestHeadCommitsBulk $ map Builds.PullRequestNumber [22201, 23463, 9999999]
  D.debugList [
      "result3:"
    , show result3
    ]
