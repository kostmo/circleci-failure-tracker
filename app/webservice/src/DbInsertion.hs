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
import qualified JsonUtils
import qualified Types
import qualified WebApi


data BackendFailureResponse = BackendFailureResponse {
    _authentication_failed :: Maybe Bool
  , _database_failed       :: Maybe Bool
  , _login_url             :: Maybe Text
  } deriving Generic

instance ToJSON BackendFailureResponse where
  toJSON = genericToJSON JsonUtils.dropUnderscore


backendFailureToResponse auth_config failure_mode =
  WebApi.JsonEither False (Just stuff) Nothing
  where
  stuff = case failure_mode of
    AuthStages.AuthFailure (AuthStages.AuthenticationFailure _maybe_login_url auth_failure_stage) -> let
         inner = BackendFailureResponse (Just True) Nothing (Just $ AuthConfig.getLoginUrl auth_config)
      in JsonUtils.ErrorDetails (AuthStages.getErrorMessage auth_failure_stage) $ Just $ toJSON inner

    AuthStages.DbFailure db_failure_reason -> let
      inner = Just $ toJSON $ BackendFailureResponse Nothing (Just True) Nothing
      in JsonUtils.ErrorDetails db_failure_reason inner


toInsertionResponse ::
     AuthConfig.GithubConfig
  -> Either (AuthStages.BackendFailure Text) Int64
  -> WebApi.JsonEither BackendFailureResponse Int64
toInsertionResponse auth_config interaction_result = case interaction_result of
  Right record_id   -> WebApi.JsonEither True Nothing $ Just record_id
  Left failure_mode -> backendFailureToResponse auth_config failure_mode

