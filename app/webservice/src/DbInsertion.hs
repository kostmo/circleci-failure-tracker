{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}


module DbInsertion where

import           Data.Aeson
import           Data.Text      (Text)
import qualified Data.Text.Lazy as TL
import           GHC.Generics
import           GHC.Int        (Int64)

import qualified AuthConfig
import qualified AuthStages
import qualified IDP
import           JsonUtils      (dropUnderscore)
import qualified Types
import qualified WebApi



data InsertionFailureResponse = InsertionFailureResponse {
    _authentication_failed :: Maybe Bool
  , _database_failed       :: Maybe Bool
  , _login_url             :: Maybe Text
  } deriving Generic

instance ToJSON InsertionFailureResponse where
  toJSON = genericToJSON dropUnderscore



toInsertionResponse ::
     AuthConfig.GithubConfig
  -> Either AuthStages.AuthenticationFailureStage (Either Text Int64)
  -> WebApi.JsonEither InsertionFailureResponse Int64
toInsertionResponse auth_config authentication_result = case authentication_result of
  Right callback_result -> case callback_result of
    Right record_id -> WebApi.JsonEither True Nothing $ Just record_id
    Left db_failure_reason -> let
      inner = InsertionFailureResponse (Nothing) (Just True) Nothing
      in WebApi.JsonEither False (Just $ WebApi.ErrorDetails db_failure_reason inner) Nothing

  Left auth_failure_stage -> let
    idp_data = IDP.mkIDPData auth_config
    inner = InsertionFailureResponse (Just True) Nothing (Just $ TL.toStrict $ Types.codeFlowUri idp_data)
    in WebApi.JsonEither False (Just $ WebApi.ErrorDetails (AuthStages.getMessage auth_failure_stage) inner) Nothing



