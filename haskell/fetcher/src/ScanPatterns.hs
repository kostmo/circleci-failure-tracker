{-# LANGUAGE OverloadedStrings #-}

module ScanPatterns where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)

import qualified DbHelpers


data MatchExpression =
    RegularExpression ByteString
  | LiteralExpression Text
  deriving Show


is_regex :: MatchExpression -> Bool
is_regex x = case x of
  RegularExpression _ -> True
  LiteralExpression _ -> False


pattern_text :: MatchExpression -> Text
pattern_text x = case x of
          RegularExpression x -> decodeUtf8 x
          LiteralExpression x -> x


data Pattern = NewPattern {
    expression       :: MatchExpression
  , description      :: Text
  , tags             :: [Text]
  , applicable_steps :: [Text]
  } deriving Show


type DbPattern = DbHelpers.WithId Pattern


data MatchSpan = NewMatchSpan {
    start :: Int
  , end   :: Int
  }


data ScanMatch = NewScanMatch {
    scanned_pattern :: DbPattern
  , line_text       :: Text
  , line_number     :: Int
  , span            :: MatchSpan
  }


pattern_list :: [Pattern]
pattern_list = [

    NewPattern (LiteralExpression "FAILED: ")
      "Ninja build failed" [] []

  , NewPattern (RegularExpression "([^\\s]+):(\\d+):(\\d+): error:")
    "Compilation error" ["compile"] []

  , NewPattern (RegularExpression "[  FAILED  ]\\s+([^\\s]+)")
     "Failed test" ["runtime"] ["Test"]

  , NewPattern (RegularExpression "[  FAILED  ]\\s+(\\d+) tests?, listed below:")
      "Failed test count" ["runtime"] ["Test"]

  , NewPattern (LiteralExpression "TypeError: ")
      "Python error" ["runtime", "python"] ["Doc Build and Push"]

  , NewPattern (LiteralExpression "AssertionError: ")
      "Test assertion failure" ["runtime", "python"] ["Test"]

  , NewPattern (LiteralExpression "CalledProcessError: Command '['ninja', '-v']' returned non-zero exit status")
      "Ninja build failure" ["build"] ["Test"]

  , NewPattern (RegularExpression "ERROR: You need Python (\\d+)\\.(\\d+) or later to use mypy")
      "Python version error for mypy" ["python"] []

  , NewPattern (LiteralExpression "ERROR: ")
      "A generic code error" [] []

  , NewPattern (LiteralExpression "ERROR: Graphs differed across invocations")
      "Graphs differ" [] []

  , NewPattern (RegularExpression "ERROR: ([^\\s]+) \\(__main__\\.(.+)\\)")
      "Test failure" [] []

  , NewPattern (LiteralExpression "Segmentation fault")
      "Segfault" ["runtime"] []

  , NewPattern (RegularExpression "find: (.+): No such file or directory")
      "find error" [] ["Build"]

  , NewPattern (LiteralExpression "unzip:  cannot find zipfile directory")
      "Unzip failed" [] []

  , NewPattern (RegularExpression "RuntimeError: test_(.+) failed!")
      "Python runtime error on test" ["runtime", "python"] ["Test"]

  , NewPattern (RegularExpression "RuntimeError: Error building extension '([^']+)'")
      "Python build error" ["build", "python"] ["Test"]

  , NewPattern (RegularExpression "RuntimeError: \\[([^:]+):(\\d+)\\] Read error \\[127.0.0.1\\]:(\\d+): Connection reset by peer")
      "Python network error" ["python"] ["Test"]

  , NewPattern (LiteralExpression "Build left local git repository checkout dirty")
      "Build dirtied the source tree" [] ["Test"]

  , NewPattern (LiteralExpression"E: Failed to fetch")
      "apt error" ["apt"] ["Set Up CI Environment After Checkout"]

  , NewPattern (LiteralExpression "E: Could not get lock /var/lib/apt/lists/lock")
      "CircleCI apt lock failure" ["infra", "apt"] []

  , NewPattern (LiteralExpression "E: Unable to acquire the dpkg frontend lock")
      "apt failure" ["infra", "apt"] []

  , NewPattern (LiteralExpression "Waiting for a VM assignment")
      "CircleCI outage" ["infra", "circleci"] ["Spin up Environment"]

  , NewPattern (LiteralExpression "Probably the package for the version we want does not exist")
      "Conda error" [] []

  , NewPattern (LiteralExpression "error: failed to push some refs to")
      "Git push failed" ["infra", "git"] ["Doc Build and Push"]

  , NewPattern (RegularExpression "Failed to recurse into submodule path '(.+)'")
      "Git submodules failure" ["infra", "git"] ["Run in docker", "Build"]
  , NewPattern (RegularExpression "::(.+) FAILED")
      "Unit test failure" ["runtine"] []

  , NewPattern (RegularExpression "fatal: unable to access '(.+)': gnutls_handshake\\(\\) failed: Error in the pull function")
      "Git fetch failed" ["infra", "git"] []

  , NewPattern (LiteralExpression "E: Unable to correct problems, you have held broken packages")
      "apt package incompatibility" ["infra", "apt"] []
  ]
