
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module ApiPost where

import           Control.Lens               hiding ((<.>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode,
                                             toJSON)
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

import qualified Builds
import qualified DbHelpers
import qualified DebugUtils                 as D
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
  -> Builds.RawCommit
  -> StatusEvent.GitHubStatusEventSetter
  -> IO (Either LT.Text StatusPostResult)
postCommitStatus
    (OAuth2.AccessToken personal_access_token)
    owned_repo
    (Builds.RawCommit target_sha1)
    status_obj = runExceptT $ do

  liftIO $ D.debugList [
      "Posting to URL:"
    , url_string
    , "with json object"
    , show json_status_obj
    ]

  either_response <- liftIO $ FetchHelpers.safeGetUrl $
    NW.postWith opts url_string json_status_obj

  {-
  liftIO $ D.debugList [
      "either_response:"
    , show either_response
    ]
  -}

  response <- except $ first LT.pack either_response

  except $ first LT.pack $ eitherDecode $ NC.responseBody response

  where
    json_status_obj = toJSON status_obj
    opts = NW.defaults
      & NW.header "Authorization" .~ ["token " <> encodeUtf8 personal_access_token]

    url_string = intercalate "/" [
        DbHelpers.githubRepoApiPrefix owned_repo
      , "statuses"
      , T.unpack target_sha1
      ]


-- | Do not use newtype; we don't want to collapse single field
data CommentBodyContainer = CommentBodyContainer {
    body         :: Text
  } deriving (Generic, Show)

instance ToJSON CommentBodyContainer



data CommentPostResult = CommentPostResult {
    id         :: Int64
  , body       :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  } deriving (Generic, Show)

instance FromJSON CommentPostResult



newtype CommentId = CommentId Int64

getPRCommentEditUrl ::
     DbHelpers.OwnerAndRepo
  -> CommentId
  -> String
getPRCommentEditUrl owned_repo (CommentId comment_id) =
  intercalate "/" [
      DbHelpers.githubRepoApiPrefix owned_repo
    , "issues"
    , "comments"
    , show comment_id
    ]


getPRCommentCreationUrl ::
     DbHelpers.OwnerAndRepo
  -> Builds.PullRequestNumber
  -> String
getPRCommentCreationUrl owned_repo (Builds.PullRequestNumber pull_request_number) =
  intercalate "/" [
      DbHelpers.githubRepoApiPrefix owned_repo
    , "issues"
    , show pull_request_number
    , "comments"
    ]


postPullRequestComment ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> Builds.PullRequestNumber
  -> Text -- ^ comment body
  -> IO (Either LT.Text CommentPostResult)
postPullRequestComment
    (OAuth2.AccessToken personal_access_token)
    owned_repo
    pull_request_number
    comment_body = runExceptT $ do

  response <- ExceptT $ fmap (first LT.pack) $ FetchHelpers.safeGetUrl $
    NW.postWith opts url_string json_to_post

  except $ first LT.pack $ eitherDecode $ NC.responseBody response

  where
    opts = NW.defaults
      & NW.header "Authorization" .~ ["token " <> encodeUtf8 personal_access_token]

    json_to_post = toJSON $ CommentBodyContainer comment_body
    url_string = getPRCommentCreationUrl owned_repo pull_request_number


updatePullRequestComment ::
     OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> CommentId
  -> Text -- ^ comment body
  -> IO (Either LT.Text CommentPostResult)
updatePullRequestComment
    (OAuth2.AccessToken personal_access_token)
    owned_repo
    comment_id
    comment_body = runExceptT $ do

  response <- ExceptT $ fmap (first LT.pack) $ FetchHelpers.safeGetUrl $
    NW.customPayloadMethodWith "PATCH" opts url_string json_to_post

  except $ first LT.pack $ eitherDecode $ NC.responseBody response

  where
    opts = NW.defaults
      & NW.header "Authorization" .~ ["token " <> encodeUtf8 personal_access_token]

    json_to_post = toJSON $ CommentBodyContainer comment_body

    url_string = getPRCommentEditUrl owned_repo comment_id

