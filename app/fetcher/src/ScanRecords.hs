module ScanRecords where

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text.Lazy             (Text)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import qualified Network.Wreq.Session       as Sess

import qualified DbHelpers
import qualified ScanPatterns


data UnidentifiedBuildFailure = NetworkProblem String | NoFailedSteps
  deriving Show


data LogInfo = LogInfo {
    log_byte_count             :: Int64
  , log_line_count             :: Int
  , log_content                :: [Text]
  , modified_by_ansi_stripping :: Bool
  , was_truncated_for_size     :: Bool
  }


data FetchingResources = FetchingResources {
    db_conn     :: Connection
  , aws_sess    :: Sess.Session
  , circle_sess :: Sess.Session
  }


newtype ScanId = ScanId Int64


data ScanTracking =
    PersistedScanId ScanId
  | NoPersistedScanId


data ScanCatchupResources = ScanCatchupResources {
    scan_id_tracking  :: ScanTracking
  , newest_pattern_id :: ScanPatterns.PatternId
  , patterns_by_id    :: HashMap Int64 ScanPatterns.Pattern
  , fetching          :: FetchingResources
  }


getPatternsWithId :: ScanCatchupResources -> [ScanPatterns.DbPattern]
getPatternsWithId = map (uncurry DbHelpers.WithId) . HashMap.toList . patterns_by_id
