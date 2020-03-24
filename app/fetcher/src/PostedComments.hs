{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostedComments where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple (FromRow)
import           GHC.Generics
import           GHC.Int                    (Int64)

import qualified AuthStages
import qualified Builds
import qualified JsonUtils


data PostedComment = PostedComment {
    _pr_number                      :: Builds.PullRequestNumber
  , _sha1                           :: Builds.RawCommit
  , _github_user_login              :: AuthStages.Username
  , _body                           :: Text
  , _created_at                     :: UTCTime
  , _updated_at                     :: UTCTime
  , _revision_count                 :: Int
  , _comment_id                     :: Int64
  , _was_new_push                   :: Bool
  , _all_no_fault_failures          :: Bool
  , _all_successful_circleci_builds :: Bool
  } deriving (Generic, FromRow)

instance ToJSON PostedComment where
  toJSON = genericToJSON JsonUtils.dropUnderscore
