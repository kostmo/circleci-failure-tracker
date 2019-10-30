{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GadgitFetch where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Aeson                 (FromJSON, eitherDecode,
                                             genericParseJSON, parseJSON)
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           GHC.Generics
import qualified Network.HTTP.Client        as NC
import           Network.Wreq               as NW

import qualified Builds
import qualified FetchHelpers
import qualified JsonUtils


data GadgitResponse = GadgitResponse {
    _result  :: [Int]
  , _success :: Bool
  , _error   :: Maybe String
  } deriving Generic

instance FromJSON GadgitResponse where
  parseJSON = genericParseJSON JsonUtils.dropUnderscore


getContainingPRs :: Builds.RawCommit -> IO (Either String [Builds.PullRequestNumber])
getContainingPRs (Builds.RawCommit sha1) = runExceptT $ do

  response <- ExceptT $ liftIO $ FetchHelpers.safeGetUrl $ NW.get url_string
  decoded_json <- except $ eitherDecode $ NC.responseBody response
  except $ if _success decoded_json
    then return $ map Builds.PullRequestNumber $ _result decoded_json
    else Left $ unwords [
        "Webservice error:"
      , Maybe.fromMaybe "<none>" $ _error decoded_json
      ]

  where
    url_string = "http://gadgit.pytorch.org/head-of-pull-requests/" <> T.unpack sha1

