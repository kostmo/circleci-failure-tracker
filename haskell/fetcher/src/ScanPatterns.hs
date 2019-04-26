{-# LANGUAGE OverloadedStrings #-}

module ScanPatterns where

import           Data.Text (Text)

import qualified DbHelpers


data Pattern = NewPattern {
    is_regex         :: Bool
  , expression       :: Text
  , description      :: Text
  , tags             :: [Text]
  , applicable_steps :: [Text]
  } deriving Show


type DbPattern = DbHelpers.WithId Pattern


data ScanMatch = NewScanMatch {
    scanned_pattern :: DbPattern
  , line_text       :: Text
  , line_number     :: Int
  , span_start      :: Int
  , span_end        :: Int
  }


pattern_list :: [Pattern]
pattern_list = [

    NewPattern False "FAILED: "
      "Ninja build failed" [] []

  , NewPattern True "([^\\s]+):(\\d+):(\\d+): error:"
    "Compilation error" ["compile"] []

  , NewPattern True "[  FAILED  ]\\s+([^\\s]+)"
     "Failed test" ["runtime"] ["Test"]

  , NewPattern True "[  FAILED  ]\\s+(\\d+) tests?, listed below:"
      "Failed test count" ["runtime"] ["Test"]

  , NewPattern False "TypeError: "
      "Python error" ["runtime", "python"] ["Doc Build and Push"]

  , NewPattern False "AssertionError: "
      "Test assertion failure" ["runtime", "python"] ["Test"]

  , NewPattern False "CalledProcessError: Command '['ninja', '-v']' returned non-zero exit status"
      "Ninja build failure" ["build"] ["Test"]

  , NewPattern True "ERROR: You need Python (\\d+)\\.(\\d+) or later to use mypy"
      "Python version error for mypy" ["python"] []

  , NewPattern False "ERROR: "
      "A generic code error" [] []

  , NewPattern False "ERROR: Graphs differed across invocations" "Graphs differ" [] []
  , NewPattern True "ERROR: ([^\\s]+) \\(__main__\\.(.+)\\)" "Test failure" [] []

  , NewPattern False "Segmentation fault"
      "Segfault" ["runtime"] []

  , NewPattern True  "find: (.+): No such file or directory"
      "find error" [] ["Build"]

  , NewPattern False "unzip:  cannot find zipfile directory"
      "Unzip failed" [] []

  , NewPattern True "RuntimeError: test_(.+) failed!"
      "Python runtime error on test" ["runtime", "python"] ["Test"]
  , NewPattern True "RuntimeError: Error building extension '([^']+)'"
      "Python build error" ["build", "python"] ["Test"]
  , NewPattern True "RuntimeError: \\[([^:]+):(\\d+)\\] Read error \\[127.0.0.1\\]:(\\d+): Connection reset by peer"
      "Python network error" ["python"] ["Test"]
  , NewPattern False "Build left local git repository checkout dirty"
      "Build dirtied the source tree" [] ["Test"]
  , NewPattern False "E: Failed to fetch"
      "apt error" ["apt"] ["Set Up CI Environment After Checkout"]

  , NewPattern False "E: Could not get lock /var/lib/apt/lists/lock"
      "CircleCI apt lock failure" ["infra", "apt"] []
  , NewPattern False "E: Unable to acquire the dpkg frontend lock"
      "apt failure" ["infra", "apt"] []
  , NewPattern False "Waiting for a VM assignment"
      "CircleCI outage" ["infra", "circleci"] ["Spin up Environment"]
  , NewPattern False "Probably the package for the version we want does not exist"
      "Conda error" [] []
  , NewPattern False "error: failed to push some refs to"
      "Git push failed" ["infra", "git"] ["Doc Build and Push"]

  , NewPattern True "Failed to recurse into submodule path '(.+)'"
      "Git submodules failure" ["infra", "git"] ["Run in docker", "Build"]
  , NewPattern True "::(.+) FAILED"
      "Unit test failure" ["runtine"] []

  , NewPattern True "fatal: unable to access '(.+)': gnutls_handshake\\(\\) failed: Error in the pull function"
      "Git fetch failed" ["infra", "git"] []

  , NewPattern False "E: Unable to correct problems, you have held broken packages"
      "apt package incompatibility" ["infra", "apt"] []
  ]
