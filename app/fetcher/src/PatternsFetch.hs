{-# LANGUAGE OverloadedStrings #-}

module PatternsFetch where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Aeson                 (eitherDecode)
import           GHC.Int                    (Int64)
import qualified Network.HTTP.Client        as NC
import           Network.Wreq               as NW

import qualified DbHelpers
import qualified FetchHelpers
import qualified SqlWrite


-- | For seeding initial data
populate_patterns ::
     DbHelpers.DbConnectionData
  -> IO (Either String [Int64])
populate_patterns connection_data = runExceptT $ do
  response <- ExceptT $ FetchHelpers.safeGetUrl $ NW.get url_string
  decoded_json <- except $ eitherDecode $ NC.responseBody response
  liftIO $ SqlWrite.restore_patterns connection_data SqlWrite.defaultPatternAuthor decoded_json

  where
    url_string = "https://raw.githubusercontent.com/kostmo/circleci-failure-tracker/master/data/patterns-dump.json"
