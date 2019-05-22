{-# LANGUAGE OverloadedStrings #-}

module FetchHelpers where

import qualified Control.Exception     as E
import           Control.Lens          hiding ((<.>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as LBS
import           Network.HTTP.Client
import           Network.Wreq          as NW


safeGetUrl ::
     IO (Response LBS.ByteString)
  -> IO (Either String (Response LBS.ByteString))
safeGetUrl f = do
  (Right <$> f) `E.catch` handler
  where
    handler :: HttpException -> IO (Either String (Response LBS.ByteString))
    handler (HttpExceptionRequest _ (StatusCodeException r _)) =
      return $ Left $ BSC.unpack $ r ^. NW.responseStatus . statusMessage
    handler x =
      return $ Left $ show x
