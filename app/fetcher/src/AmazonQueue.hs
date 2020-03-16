{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module AmazonQueue where

import           Data.Aeson                  (ToJSON)
import           Data.Aeson.Text             (encodeToLazyText)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (toStrict)

import qualified AmazonQueueData
import           Control.Lens                hiding ((<.>))
import qualified Control.Monad.Trans.AWS     as CMTA
import           Network.AWS                 (send)
import qualified Network.AWS.Easy            as AE
import qualified Network.AWS.SQS             as SQS
import qualified Network.AWS.SQS.SendMessage as MySendMessage


AE.wrapAWSService 'SQS.sqs "SQSService" "SQSSession"


doSendMessage ::
     AmazonQueueData.QueueURL
  -> Text -- ^ group ID
  -> Text -- ^ message_text
  -> SQSSession
  -> IO MySendMessage.SendMessageResponse
doSendMessage (AmazonQueueData.QueueURL s) group_id m =
  AE.withAWS $ send $
    SQS.sendMessage s m
      & (MySendMessage.smMessageGroupId ?~ group_id)


sendSqsMessage :: ToJSON a =>
     AmazonQueueData.QueueURL
  -> Text -- ^ group ID
  -> a
  -> IO (Maybe Text)
sendSqsMessage queueURL group_id my_msg_json = do

  sqsSession <- AE.connect awsInfo sqsService

  foo <- doSendMessage
    queueURL
    group_id
    my_msg_text
    sqsSession

  return $ foo ^. MySendMessage.smrsMessageId
  where
    my_msg_text = toStrict $ encodeToLazyText my_msg_json


    awsInfo = AE.awsConfig $ AE.AWSRegion CMTA.Ohio
