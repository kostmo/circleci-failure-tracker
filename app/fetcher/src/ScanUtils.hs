module ScanUtils where

import           Data.Array                ((!))
import           Data.Maybe                (Maybe)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Text.Internal.Search as Search
import qualified Safe
import           Text.Regex.Base
import           Text.Regex.PCRE           ((=~~))

import qualified DbHelpers
import qualified ScanPatterns
import           SillyMonoids              ()


applySinglePattern ::
     (Int, T.Text)
  -> ScanPatterns.DbPattern
  -> Maybe ScanPatterns.ScanMatch
applySinglePattern (line_number, line) db_pattern =
  match_partial <$> match_span
  where
    pattern_obj = DbHelpers.record db_pattern

    match_span = case ScanPatterns.expression pattern_obj of
      ScanPatterns.RegularExpression regex_text _ -> do
        (match_offset, match_length) <- T.unpack line =~~ encodeUtf8 regex_text :: Maybe (MatchOffset, MatchLength)
        return $ ScanPatterns.NewMatchSpan match_offset (match_offset + match_length)
      ScanPatterns.LiteralExpression literal_text -> do
        first_index <- Safe.headMay (Search.indices literal_text line)
        return $ ScanPatterns.NewMatchSpan first_index (first_index + T.length literal_text)

    match_partial = ScanPatterns.NewScanMatch db_pattern .
      ScanPatterns.NewMatchDetails
        line
        line_number


getFirstMatchGroup :: T.Text -> T.Text -> Maybe String
getFirstMatchGroup extracted_chunk regex_text = do
  (_before, match_array, _after) <- maybe_match
  return $ fst $ match_array ! 1
  where
    maybe_match = (T.unpack extracted_chunk =~~ encodeUtf8 regex_text) :: Maybe (String, MatchText String, String)
