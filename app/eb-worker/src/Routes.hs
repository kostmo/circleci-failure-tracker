{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Except      (ExceptT (ExceptT), except,
                                                  runExceptT)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Aeson                      (ToJSON)
import           Data.Default                    (def)
import           Data.Either.Utils               (maybeToEither)
import           Data.String                     (fromString)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Time.Clock                 as Clock
import qualified Data.Vault.Lazy                 as Vault
import           Log                             (LogT, localDomain)
import           Network.Wai
import           Network.Wai.Log                 (logRequestsWith)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.Gzip     (gzip)
import           Network.Wai.Middleware.Static   hiding ((<|>))
import           Network.Wai.Session             (Session, SessionStore,
                                                  withSession)
import           System.FilePath
import qualified Web.Scotty                      as S
import qualified Web.Scotty.Internal.Types       as ScottyTypes

import qualified AuthConfig
import qualified AuthStages
import qualified Builds
import qualified DbHelpers
import qualified GitRev
import qualified JsonUtils
import qualified MatchOccurrences
import qualified Scanning
import qualified ScanPatterns
import qualified SqlRead
import qualified SqlUpdate
import qualified SqlWrite
import qualified StatusUpdate
import qualified Types
import qualified WebApi


data SetupData = SetupData {
    _setup_static_base           :: String
  , _setup_github_config         :: AuthConfig.GithubConfig
  , _setup_connection_data       :: DbHelpers.DbConnectionData
  , _setup_mview_connection_data :: DbHelpers.DbConnectionData -- ^ for updating materialized views
  }


data PersistenceData = PersistenceData {
    _setup_cache   :: Types.CacheStore
  , _setup_session :: Vault.Key (Session IO String String)
  , _setup_store   :: SessionStore IO String String
  }


scottyApp ::
     (LogT IO () -> IO ())
  -> SetupData
  -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp
    _logger
    (SetupData static_base github_config connection_data mview_connection_data) = do


  S.post "/worker/scheduled-work" $ do

    rq <- S.request
    let path_string = T.intercalate "/" $ pathInfo rq


    start_id <- liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      SqlWrite.insertEbWorkerStart conn path_string "scanning builds"


    S.json [("hello-post" :: Text)]

    liftIO $ do
      conn <- DbHelpers.get_connection connection_data
      SqlWrite.insertEbWorkerFinish conn start_id
