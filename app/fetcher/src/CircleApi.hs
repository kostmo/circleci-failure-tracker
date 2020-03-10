module CircleApi where

import           Data.Text       (Text)
import           Web.JWT         as JWT

import qualified AmazonQueueData


newtype CircleCIApiToken = CircleCIApiToken Text


data ThirdPartyAuth = ThirdPartyAuth {
    circle_api_token :: CircleCIApiToken
  , jwt_signer       :: JWT.Signer
  , sqs_queue_url    :: AmazonQueueData.QueueURL
  }
