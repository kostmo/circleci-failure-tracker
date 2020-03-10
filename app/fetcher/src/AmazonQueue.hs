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
  -> Text
  -> SQSSession
  -> IO MySendMessage.SendMessageResponse
doSendMessage (AmazonQueueData.QueueURL s) m =
  AE.withAWS $ send $
    SQS.sendMessage s m
      & MySendMessage.smMessageGroupId .~ Just "foobar"


sendSqsMessage :: ToJSON a => AmazonQueueData.QueueURL -> a -> IO (Maybe Text)
sendSqsMessage queueURL my_msg_json = do

  sqsSession <- AE.connect awsInfo sqsService

  foo <- doSendMessage queueURL my_msg_text sqsSession

  return $ foo ^. MySendMessage.smrsMessageId
  where
    my_msg_text = toStrict $ encodeToLazyText my_msg_json


    awsInfo = AE.awsConfig $ AE.AWSRegion CMTA.Ohio
