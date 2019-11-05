{-# LANGUAGE OverloadedStrings #-}

module Constants where

import           Data.ByteString (ByteString)
import           Data.List       (intercalate)
import           Data.Text       (Text)

import qualified AuthStages


printDebug :: Bool
printDebug = True


masterName :: Text
masterName = "master"


gitCommitPrefixLength :: Int
gitCommitPrefixLength = 7


defaultPatternAuthor :: AuthStages.Username
defaultPatternAuthor = AuthStages.Username "kostmo"


presumedGoodBranches :: [Text]
presumedGoodBranches = [
    masterName
  , "pull/18339"
  , "pull/18340"
  , "pull/18341"
  , "pull/18342"
  , "pull/18343"
  , "pull/18907"
  ]


-- | Not used
appName :: FilePath
appName = "circleci-failure-tracker"


jsonMimeType :: ByteString
jsonMimeType = "application/json"


projectName :: String
projectName = "pytorch"


repoName :: String
repoName = "pytorch"


circleci_api_base :: String
circleci_api_base = intercalate "/"
  [ "https://circleci.com/api/v1.1/project/github"
  , projectName
  , repoName
  ]


