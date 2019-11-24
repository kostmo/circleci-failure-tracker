{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PostedComments where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple (FromRow)
import           GHC.Generics

import qualified AuthStages
import qualified Builds
import qualified JsonUtils


data PostedComment = PostedComment {
    _pr_number         :: Builds.PullRequestNumber
  , _sha1              :: Builds.RawCommit
  , _github_user_login :: AuthStages.Username
  , _body              :: Text
  , _created_at        :: UTCTime
  , _created_at        :: UTCTime
  , _revision_count    :: Int
  } deriving (Generic, FromRow)

instance ToJSON PostedComment where
  toJSON = genericToJSON JsonUtils.dropUnderscore
