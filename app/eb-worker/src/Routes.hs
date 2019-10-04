{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import           Data.Time                 (parseTimeM)
import           Data.Time.Format          (defaultTimeLocale,
                                            iso8601DateFormat)
import           GHC.Int                   (Int64)
import           Log                       (LogT)
import           Network.Wai
import           Text.Read                 (readMaybe)
import qualified Web.Scotty                as S
import qualified Web.Scotty.Internal.Types as ScottyTypes

import qualified AuthConfig
import qualified BuildRetrieval
import qualified DbHelpers
import qualified MyUtils
import qualified SqlWrite


-- | 2 minutes
statementTimeoutSeconds :: Integer
statementTimeoutSeconds = 120


data SetupData = SetupData {
    _setup_static_base           :: String
  , _setup_github_config         :: AuthConfig.GithubConfig
  , _setup_connection_data       :: DbHelpers.DbConnectionData
  , _setup_mview_connection_data :: DbHelpers.DbConnectionData -- ^ for updating materialized views
  }


wrapWithDbDurationRecords ::
     DbHelpers.DbConnectionData
  -> (Int64 -> ScottyTypes.ActionT LT.Text IO ())
  -> ScottyTypes.ActionT LT.Text IO ()
wrapWithDbDurationRecords connection_data func = do
  rq <- S.request
  let path_string = T.intercalate "/" $ pathInfo rq

  maybe_task_name <- S.header "X-Aws-Sqsd-Taskname"
  maybe_scheduled_at <- S.header "X-Aws-Sqsd-Scheduled-At"
  maybe_sender_id <- S.header "X-Aws-Sqsd-Sender-Id"

  liftIO $ MyUtils.debugList [
      "KARL -- maybe_task_name:"
    , show maybe_task_name
    , "maybe_scheduled_at:"
    , show maybe_scheduled_at
    , "maybe_sender_id:"
    , show maybe_sender_id
    ]


  let maybe_parsed_time = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) . init . LT.unpack =<< maybe_scheduled_at
      maybe_cron_headers = SqlWrite.BeanstalkCronHeaders
       <$> maybe_task_name
       <*> maybe_parsed_time
       <*> maybe_sender_id

  start_id <- liftIO $ do
    putStrLn "Starting timed database operation..."
    conn <- DbHelpers.getConnectionWithStatementTimeout connection_data statementTimeoutSeconds
    SqlWrite.insertEbWorkerStart
      conn
      path_string
      "scanning builds"
      maybe_cron_headers

  func start_id

  liftIO $ do
    conn <- DbHelpers.getConnectionWithStatementTimeout connection_data statementTimeoutSeconds
    SqlWrite.insertEbWorkerFinish conn start_id
    putStrLn "Finished timed database operation..."


scottyApp ::
     (LogT IO () -> IO ())
  -> SetupData
  -> ScottyTypes.ScottyT LT.Text IO ()
scottyApp
    _logger
    (SetupData static_base github_config connection_data mview_connection_data) = do


  S.post "/worker/scheduled-work" $ do

    wrapWithDbDurationRecords connection_data $ \eb_worker_event_id -> do

      liftIO $ do

        putStrLn "Starting CircleCI build retrieval..."

        conn <- DbHelpers.getConnectionWithStatementTimeout connection_data statementTimeoutSeconds

        BuildRetrieval.updateCircleCIBuildsList
          conn
          (Just eb_worker_event_id)
          BuildRetrieval.Completed
          0
          ["master"]
          100000

        putStrLn "Finished CircleCI build retrieval."


      S.json [("hello-post" :: Text)]



  S.post "/worker/update-pr-associations" $ do

    wrapWithDbDurationRecords connection_data $ \eb_worker_event_id -> do

      liftIO $ do

        putStrLn "Starting PR association retrieval..."

        conn <- DbHelpers.getConnectionWithStatementTimeout connection_data statementTimeoutSeconds
        putStrLn "TODO: Doing nothing because we don't have a Git repo!"

        putStrLn "Finished association retrieval."


      S.json [("hello-post" :: Text)]

