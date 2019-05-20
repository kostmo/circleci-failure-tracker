{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module ScanPatterns where

import           Data.Aeson
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics

import qualified DbHelpers


data MatchExpression =
    RegularExpression
      ByteString
      Bool -- ^ has nondeterminisitic values (e.g. timestamps; some capture groups are still deterministic)
  | LiteralExpression Text
  deriving (Show, Generic)

instance ToJSON MatchExpression


instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8


is_regex :: MatchExpression -> Bool
is_regex x = case x of
  RegularExpression _ _ -> True
  LiteralExpression _   -> False


pattern_text :: MatchExpression -> Text
pattern_text = \case
  RegularExpression x _ -> decodeUtf8 x
  LiteralExpression x -> x


data Pattern = NewPattern {
    expression       :: MatchExpression
  , description      :: Text
  , tags             :: [Text]
  , applicable_steps :: [Text]
  , specificity      :: Int
  , is_retired       :: Bool
  } deriving (Generic, Show)

instance ToJSON Pattern


type DbPattern = DbHelpers.WithId Pattern


data MatchSpan = NewMatchSpan {
    start :: Int
  , end   :: Int
  } deriving Generic

instance ToJSON MatchSpan


data MatchDetails = NewMatchDetails {
    line_text   :: Text
  , line_number :: Int
  , span        :: MatchSpan
  } deriving Generic

instance ToJSON MatchDetails


data ScanMatch = NewScanMatch {
    scanned_pattern :: DbPattern
  , match_details   :: MatchDetails
  } deriving Generic

instance ToJSON ScanMatch


pattern_list :: [Pattern]
pattern_list = [

    NewPattern (LiteralExpression "FAILED: ")
      "Ninja build failed" [] [] 5 False

  , NewPattern (LiteralExpression "No such file or directory")
      "Missing file" [] [] 2 False

  , NewPattern (LiteralExpression "WARNING: infoROM is corrupted")
      "CircleCI failure" ["infra", "flaky"] [] 3 False

  , NewPattern (LiteralExpression "unexpected EOF")
      "Docker problem" ["docker"] [] 2 False

  , NewPattern (LiteralExpression "received unexpected HTTP status")
      "Docker problem" ["docker"] [] 2 False

  , NewPattern (LiteralExpression "Could not install packages due to an EnvironmentError")
      "env problem" ["python"] [] 2 False

  , NewPattern (RegularExpression "([^\\s]+):(\\d+):(\\d+): error:" True)
    "Compilation error" ["compile"] [] 5 False

  , NewPattern (RegularExpression "\\[  FAILED  \\]\\s+([^\\s]+)" True)
     "Failed test" ["runtime"] ["Test"] 3 False

  , NewPattern (RegularExpression "\\[  FAILED  \\]\\s+(\\d+) tests?, listed below:" True)
      "Failed test count" ["runtime"] ["Test"] 2 False

  , NewPattern (LiteralExpression "TypeError: ")
      "Python error" ["runtime", "python"] ["Doc Build and Push"] 2 False

  , NewPattern (LiteralExpression "AssertionError: ")
      "Test assertion failure" ["runtime", "python"] ["Test"] 2 False

  , NewPattern (LiteralExpression "CalledProcessError: Command '['ninja', '-v']' returned non-zero exit status")
      "Ninja build failure" ["build"] ["Test"] 5 False

  , NewPattern (RegularExpression "ERROR: You need Python (\\d+)\\.(\\d+) or later to use mypy" False)
      "Python version error for mypy" ["python"] [] 1 False

  , NewPattern (LiteralExpression "ERROR: ")
      "A generic code error" [] [] 1 False

  , NewPattern (LiteralExpression "ERROR: Graphs differed across invocations")
      "Graphs differ" ["flaky"] [] 5 False

  , NewPattern (RegularExpression "ERROR: ([^\\s]+) \\(__main__\\.(.+)\\)" False)
      "Test failure" [] [] 4 False

  , NewPattern (LiteralExpression "Segmentation fault")
      "Segfault" ["runtime"] [] 5 False

  , NewPattern (RegularExpression "find: (.+): No such file or directory" True)
      "find error" [] ["Build"] 1 False

  , NewPattern (LiteralExpression "unzip:  cannot find zipfile directory")
      "Unzip failed" [] [] 2 False

  , NewPattern (LiteralExpression "ERROR: test_proper_exit (__main__.TestDataLoader)")
      "test_proper_exit" ["flaky"] [] 7 False

  , NewPattern (RegularExpression "RuntimeError: test_(.+) failed!" False)
      "Python runtime error on test" ["runtime", "python"] ["Test"] 3 False

  , NewPattern (RegularExpression "RuntimeError: Error building extension '([^']+)'" False)
      "Python build error" ["build", "python"] ["Test"] 3 False

  , NewPattern (RegularExpression "RuntimeError: \\[([^:]+):(\\d+)\\] Read error \\[127.0.0.1\\]:(\\d+): Connection reset by peer" True)
      "Python network error" ["python", "flaky"] ["Test"] 5 False

  , NewPattern (LiteralExpression "Build left local git repository checkout dirty")
      "Build dirtied the source tree" [] ["Test"] 2 False

  , NewPattern (LiteralExpression"E: Failed to fetch")
      "apt error" ["apt"] ["Set Up CI Environment After Checkout"] 5 False

  , NewPattern (LiteralExpression "E: Could not get lock /var/lib/apt/lists/lock")
      "CircleCI apt lock failure" ["infra", "apt"] [] 5 False

  , NewPattern (LiteralExpression "E: Unable to acquire the dpkg frontend lock")
      "apt failure" ["infra", "apt"] [] 5 False

  , NewPattern (LiteralExpression "Waiting for a VM assignment")
      "CircleCI outage" ["infra", "circleci", "flaky"] ["Spin up Environment"] 5 False

  , NewPattern (LiteralExpression "Probably the package for the version we want does not exist")
      "Conda error" [] [] 5 False

  , NewPattern (LiteralExpression "error: failed to push some refs to")
      "Git push failed" ["infra", "git", "flaky"] ["Doc Build and Push"] 5 False

  , NewPattern (RegularExpression "Failed to recurse into submodule path '(.+)'" False)
      "Git submodules failure" ["infra", "git", "flaky"] ["Run in docker", "Build"] 5 False

  , NewPattern (RegularExpression "::(.+) FAILED" False)
      "Unit test failure" ["runtime"] [] 3 False

  , NewPattern (RegularExpression "fatal: unable to access '(.+)': gnutls_handshake\\(\\) failed: Error in the pull function" True)
      "Git fetch failed" ["infra", "git"] [] 2 False

  , NewPattern (LiteralExpression "E: Unable to correct problems, you have held broken packages")
      "apt package incompatibility" ["infra", "apt"] [] 5 False
  ]
