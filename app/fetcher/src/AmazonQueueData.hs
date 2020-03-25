{-# LANGUAGE DeriveGeneric #-}

module AmazonQueueData where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Text      (Text)
import qualified Data.Text.Lazy as TL
import           GHC.Generics   (Generic)

import qualified Builds


newtype SqsMessageId = NewSqsMessageId (Maybe TL.Text)

newtype MessageGroupId = MessageGroupId Text


newtype QueueURL = QueueURL Text deriving Show


data SqsBuildScanMessage = SqsBuildScanMessage {
    sha1 :: Builds.RawCommit
  , msg  :: Text
  } deriving (Show, Generic)

instance ToJSON SqsBuildScanMessage
instance FromJSON SqsBuildScanMessage

