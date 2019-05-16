module ScanRecords where

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Int                    (Int64)
import qualified Network.Wreq.Session       as Sess
import Data.Text (Text)

import qualified DbHelpers
import qualified ScanPatterns


data PatternId = NewPatternId Int64


data UnidentifiedBuildFailure = NetworkProblem String | NoFailedSteps


data LogInfo = LogInfo {
    log_byte_count :: Int
  , log_line_count :: Int
  , log_content :: Text
  }


data FetchingResources = FetchingResources {
    db_conn     :: Connection
  , aws_sess    :: Sess.Session
  , circle_sess :: Sess.Session
  , cache_dir   :: FilePath
  }


data ScanCatchupResources = ScanCatchupResources {
    scan_id           :: Int64
  , newest_pattern_id :: PatternId
  , patterns_by_id    :: HashMap Int64 ScanPatterns.Pattern
  , fetching          :: FetchingResources
  }


get_patterns_with_id :: ScanCatchupResources -> [ScanPatterns.DbPattern]
get_patterns_with_id = map (uncurry DbHelpers.WithId) . HashMap.toList . patterns_by_id
