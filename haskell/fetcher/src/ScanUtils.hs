{-# LANGUAGE OverloadedStrings #-}

module ScanUtils where

import           Data.Maybe                (Maybe)
import qualified Data.Text                 as T
import qualified Data.Text.Internal.Search as Search
import qualified Safe
import           System.FilePath
import           Text.Regex.Base
import           Text.Regex.PCRE           ((=~~))

import qualified Builds
import qualified Constants
import qualified DbHelpers
import qualified ScanPatterns
import           SillyMonoids              ()


apply_single_pattern ::
     (Int, T.Text)
  -> ScanPatterns.DbPattern
  -> Maybe ScanPatterns.ScanMatch
apply_single_pattern (line_number, line) db_pattern = match_partial <$> match_span
  where
    match_span = case ScanPatterns.expression pattern_obj of
      ScanPatterns.RegularExpression regex_text -> case ((T.unpack line) =~~ regex_text :: Maybe (MatchOffset, MatchLength)) of
        Just (match_offset, match_length) -> Just $ ScanPatterns.NewMatchSpan match_offset (match_offset + match_length)
        Nothing -> Nothing
      ScanPatterns.LiteralExpression literal_text -> case Safe.headMay (Search.indices literal_text line) of
        Just first_index -> Just $ ScanPatterns.NewMatchSpan first_index (first_index + T.length literal_text)
        Nothing -> Nothing

    match_partial x = ScanPatterns.NewScanMatch db_pattern $
      ScanPatterns.NewMatchDetails
        line
        line_number
        x
    pattern_obj = DbHelpers.record db_pattern


gen_log_path :: Builds.BuildNumber -> IO FilePath
gen_log_path (Builds.NewBuildNumber build_num) = do
  cache_dir <- Constants.get_url_cache_basedir
  return $ cache_dir </> filename_stem <.> "log"
  where
    filename_stem = show build_num
