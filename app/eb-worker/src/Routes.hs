{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Time                  (parseTimeM)
import qualified Data.Time.Clock            as Clock
import           Data.Time.Format           (defaultTimeLocale,
                                             iso8601DateFormat)
import           GHC.Generics               (Generic)
import           GHC.Int                    (Int64)
import           Log                        (LogT)
import qualified Network.OAuth.OAuth2       as OAuth2
import           Network.Wai
import qualified Web.Scotty                 as S
import qualified Web.Scotty.Internal.Types  as ScottyTypes

import qualified AuthConfig
import qualified BuildRetrieval
import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified DebugUtils                 as D
import qualified Scanning
import qualified SqlRead
import qualified SqlWrite
import qualified StatusUpdate


-- | 2 minutes
statementTimeoutSeconds :: Integer
statementTimeoutSeconds = 120


data SqsBuildScanMessage = SqsBuildScanMessage {
    sha1 :: Text
  , msg  :: Text
  } deriving (Show, Generic)

instance ToJSON SqsBuildScanMessage
instance FromJSON SqsBuildScanMessage



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

  liftIO $ D.debugList [
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
    (SetupData _static_base github_config connection_data _mview_connection_data) = do


  S.post "/worker/scheduled-work" $ do

    wrapWithDbDurationRecords connection_data $ \eb_worker_event_id -> do

      liftIO $ do

        current_time <- Clock.getCurrentTime
        D.debugList [
            "Starting CircleCI build retrieval as"
          , show current_time
          ]

        conn <- DbHelpers.getConnectionWithStatementTimeout
          connection_data
          statementTimeoutSeconds

        insertion_count <- BuildRetrieval.updateCircleCIBuildsList
          conn
          (Just eb_worker_event_id)
          BuildRetrieval.Completed
          0
          ["master"]
          100000

        D.debugList [
            "Finished CircleCI build retrieval."
          , "Inserted"
          , show insertion_count
          , "records."
          ]

      S.json ["hello-post" :: Text]


  S.post "/worker/update-pr-associations" $

    wrapWithDbDurationRecords connection_data $ \_eb_worker_event_id -> do

      liftIO $ do
        current_time <- Clock.getCurrentTime
        D.debugList [
            "Starting PR association retrieval at"
          , show current_time
          ]

        conn <- DbHelpers.getConnectionWithStatementTimeout connection_data statementTimeoutSeconds

        either_result <- SqlWrite.updateMergedPullRequestHeadCommits conn
        case either_result of
          Right association_list -> D.debugList [
              "Retrieved"
            , show $ length association_list
            , "associations"
            ]
          Left msg -> D.debugList [
              "Failed retrieving PR assciations:"
            , T.unpack msg
            ]

        D.debugStr "Finished association retrieval."


      S.json ["hello-post" :: Text]


  S.post "/worker/scan-sha1" $ do



    liftIO $ do
      current_time <- Clock.getCurrentTime
      D.debugList [
          "Posted to:"
        , "/worker/scan-sha1"
        , "at"
        , show current_time
        ]

    body_json <- S.jsonData

    wrapWithDbDurationRecords connection_data $ \_ -> do

      liftIO $ doStuff
        connection_data
        (AuthConfig.personal_access_token github_config)
        (DbHelpers.OwnerAndRepo Constants.projectName Constants.repoName)
        body_json

    S.json ["hello-post" :: Text]


doStuff ::
     DbHelpers.DbConnectionData
  -> OAuth2.AccessToken
  -> DbHelpers.OwnerAndRepo
  -> SqsBuildScanMessage
  -> IO ()
doStuff
    connection_data
    access_token
    owned_repo
    body_json = do

  D.debugList [
      "Starting sha1 scan of"
    , show commit_sha1
    ]

  conn <- DbHelpers.getConnectionWithStatementTimeout
    connection_data
    statementTimeoutSeconds

  scan_resources <- Scanning.prepareScanResources conn Nothing

  universal_builds <- runReaderT
    (SqlRead.getUnvisitedBuildsForSha1 commit_sha1)
    conn

  D.debugList [
      "Unvisited build IDs:"
    , show $ map DbHelpers.db_id universal_builds
    ]


  -- TODO Replace this pair of functions with scanAndPost?
  scan_matches <- Scanning.processUnvisitedBuilds
    scan_resources
    universal_builds

  runExceptT $ StatusUpdate.postCommitSummaryStatus
    conn
    access_token
    owned_repo
    commit_sha1

  D.debugList [
      "Scan match count:"
    , show $ length scan_matches
    ]


  either_deletion_count <- runReaderT (SqlWrite.deleteSha1QueuePlaceholder commit_sha1) conn

  D.debugList [
      "Removed"
    , show either_deletion_count
    , "sha1's from queue"
    ]

  putStrLn "Finished sha1 scan."

  where
    commit_sha1 = Builds.RawCommit $ sha1 body_json




