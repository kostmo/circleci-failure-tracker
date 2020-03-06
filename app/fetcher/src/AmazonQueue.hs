{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module AmazonQueue where

import           Control.Monad           (void)
import           Data.Text               (Text)

--import           Control.Lens            hiding ((<.>))
import qualified Control.Monad.Trans.AWS as CMTA
import           Network.AWS             (send)
import qualified Network.AWS.Easy        as AE
import qualified Network.AWS.SQS         as SQS


AE.wrapAWSService 'SQS.sqs "SQSService" "SQSSession"


doSendMessage :: QueueURL -> Text -> SQSSession -> IO ()
doSendMessage (QueueURL s) m = AE.withAWS $ do
    void $ send $ SQS.sendMessage s m

newtype QueueURL = QueueURL Text deriving Show

sendSqsMessage my_msg_text = do

  sqsSession <- AE.connect
                  (AE.awsConfig (AE.AWSRegion CMTA.Ohio))
                  sqsService

  doSendMessage queueURL my_msg_text sqsSession
--      &

  where
    queueURL = QueueURL "https://sqs.us-east-2.amazonaws.com/308535385114/github-status-event-sha1-scanning-queue.fifo"
--    awsInfo = AE.awsConfig $ AE.AWSRegion CMTA.Ohio
