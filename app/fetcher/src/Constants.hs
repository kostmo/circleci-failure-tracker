{-# LANGUAGE OverloadedStrings #-}

module Constants where

import           Data.ByteString (ByteString)
import           Data.List       (intercalate)
import           Data.Text       (Text)

import qualified AuthStages
import qualified DbHelpers


printDebug :: Bool
printDebug = True


masterName :: Text
masterName = "master"


gitCommitPrefixLength :: Int
gitCommitPrefixLength = 8


defaultPatternAuthor :: AuthStages.Username
defaultPatternAuthor = AuthStages.Username "kostmo"


-- | Not used
appName :: FilePath
appName = "circleci-failure-tracker"


jsonMimeType :: ByteString
jsonMimeType = "application/json"


projectName :: String
projectName = "pytorch"


repoName :: String
repoName = "pytorch"


pytorchOwnedRepo :: DbHelpers.OwnerAndRepo
pytorchOwnedRepo = DbHelpers.OwnerAndRepo
  projectName
  repoName


circleciApiBase :: String
circleciApiBase = intercalate "/"
  [ "https://circleci.com/api/v1.1/project/github"
  , projectName
  , repoName
  ]


