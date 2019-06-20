{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiPost where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Aeson                 (FromJSON, eitherDecode, toJSON)
import           Data.Bifunctor             (first)
import           Data.List                  (intercalate)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Lazy             as LT
import           Data.Time                  (UTCTime)
import           GHC.Generics
import           GHC.Int                    (Int64)
import qualified Network.HTTP.Client        as NC
import qualified Network.OAuth.OAuth2       as OAuth2
import           Network.Wreq               as NW

import qualified DbHelpers
import qualified FetchHelpers
import qualified StatusEvent


data StatusPostResult = StatusPostResult {
    id          :: Int64
  , url         :: Text
  , state       :: Text
  , description :: Text
  , target_url  :: Text
  , context     :: Text
  , created_at  :: UTCTime
  , updated_at  :: UTCTime
  } deriving (Generic, Show)

instance FromJSON StatusPostResult


postCommitStatus ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> T.Text
  -> StatusEvent.GitHubStatusEventSetter
  -> IO (Either LT.Text StatusPostResult)
postCommitStatus
    (OAuth2.AccessToken personal_access_token)
    owned_repo
    target_sha1
    status_obj = runExceptT $ do

  response <- ExceptT $ fmap (first LT.pack) $ FetchHelpers.safeGetUrl $
    NW.postWith opts url_string $ toJSON status_obj

  except $ first LT.pack $ eitherDecode $ NC.responseBody response

  where
    opts = NW.defaults
      & NW.header "Authorization" .~ ["token " <> encodeUtf8 personal_access_token]

    url_string = intercalate "/" [
        DbHelpers.githubRepoApiPrefix owned_repo
      , "statuses"
      , T.unpack target_sha1
      ]



