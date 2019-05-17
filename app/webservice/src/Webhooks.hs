{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Webhooks where
import           Data.Aeson
import           Data.Text.Lazy (Text)
import           Data.Time      (UTCTime)
import           GHC.Generics

data GitHubStatusEvent = GitHubStatusEvent {
    sha         :: Text
  , name        :: Text -- ^ <org>/<repo>
  , description :: Text
  , state       :: Text
  , target_url  :: Text
  , context     :: Text
  , created_at  :: UTCTime
  , updated_at  :: UTCTime
  } deriving Generic

instance FromJSON GitHubStatusEvent

--instance ToJSON GitHubStatusEvent where
--  toJSON = genericToJSON JsonUtils.dropUnderscore
