{-# LANGUAGE OverloadedStrings #-}

module PatternsFetch where

import           Data.Aeson          (eitherDecode)
import           Data.Bifunctor      (first)
import qualified Data.Text.Lazy      as LT
import           GHC.Int             (Int64)
import qualified Network.HTTP.Client as NC
import           Network.Wreq        as NW

import qualified DbHelpers
import qualified FetchHelpers
import qualified SqlWrite


-- | For seeding initial data
populate_patterns ::
     DbHelpers.DbConnectionData
  -> IO (Either LT.Text [Int64])
populate_patterns connection_data = do

  either_r <- FetchHelpers.safeGetUrl $ NW.get url_string
  case either_r of
    Left err_string -> return $ Left $ LT.pack err_string
    Right response -> do
      let response_body = NC.responseBody response
      either_decoded_json <- return $ first LT.pack $ eitherDecode response_body
      case either_decoded_json of
        Left err_string -> return $ Left err_string
        Right decoded_json -> do
          result <- SqlWrite.restore_patterns connection_data SqlWrite.defaultPatternAuthor decoded_json
          return $ Right result

  where
    url_string = "https://raw.githubusercontent.com/kostmo/circleci-failure-tracker/master/data/patterns-dump.json"
