{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module ApiPost where

import           Control.Lens              hiding ((<.>))
import           Control.Monad.Error.Class
import           Data.Aeson                (toJSON)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import qualified Network.OAuth.OAuth2      as OAuth2
import           Network.Wai               (Request, vault)
import           Network.Wai.Session       (Session)
import           Network.Wreq              as NW
import qualified Network.Wreq.Session      as Sess
import           Prelude
import           URI.ByteString            (parseURI, strictURIParserOptions)
import           Web.Scotty
import           Web.Scotty.Internal.Types

import qualified AuthConfig
import qualified AuthStages
import qualified FetchHelpers
import qualified Github
import qualified Keys
import           Session
import           Types
import           Utils
import qualified Webhooks


data OwnerAndRepo = OwnerAndRepo {
    owner :: String
  , repo  :: String
  }


postCommitStatus ::
     T.Text
  -> OwnerAndRepo
  -> T.Text
  -> Webhooks.GitHubStatusEventSetter
  -> IO (Either T.Text ())
postCommitStatus personal_access_token owned_repo target_sha1 status_obj = do

  either_r <- FetchHelpers.safeGetUrl $ NW.postWith opts url_string $ toJSON status_obj
  return $ Right ()

  where
    opts = NW.defaults
      & NW.header "Authorization" .~ ["token " <> encodeUtf8 personal_access_token]

    url_string :: String
    url_string = "https://api.github.com/repos/"
      <> owner owned_repo
      <> "/"
      <> repo owned_repo
      <> "/statuses/"
      <> T.unpack target_sha1
--    url_string = "http://localhost:3001/api/echo"



