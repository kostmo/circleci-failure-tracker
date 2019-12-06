{-# LANGUAGE LambdaCase #-}

module ScanUtils where

--import           Control.Concurrent        (threadDelay)
import           Data.Array                ((!))
import           Data.Either.Utils         (maybeToEither)
import           Data.Maybe                (Maybe)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Text.Internal.Search as Search
import qualified Data.Text.Lazy            as LT
import qualified Safe
import           Text.Regex.Base
import           Text.Regex.PCRE           ((=~~))

import qualified DbHelpers
--import qualified DebugUtils                as D
import qualified ScanPatterns
import           SillyMonoids              ()
import           System.Timeout            (timeout)


-- | 5 seconds
lineScanTimeoutMicroseconds :: Int
lineScanTimeoutMicroseconds = 1000000 * 5


data PatternScanTimeout = PatternScanTimeout Int (Int, LT.Text) ScanPatterns.DbPattern


-- | The timeout protects against adversarial/pathological inputs, such
-- as single lines that are 500K characters long, which freeze certain regexes
--
-- FIXME The timeout doesn't seem to prevent PCRE from freezing.
applySinglePatternIO ::
     (Int, LT.Text)
  -> ScanPatterns.DbPattern
  -> IO (Either PatternScanTimeout MatchAnswer)
applySinglePatternIO tup@(_num, _line_text) db_pattern = do

  maybe_result <- timeout lineScanTimeoutMicroseconds $ do
    {-
    D.debugList [
        "\t\tScanning with pattern:"
      , show $ DbHelpers.db_id db_pattern
      ]

    D.debugStr "Starting to sleep..."
    threadDelay 7000000
    D.debugStr "Finished sleep."
    -}
    let answer = applySinglePattern tup db_pattern
    return $! answer

  return $! maybeToEither (PatternScanTimeout lineScanTimeoutMicroseconds tup db_pattern) maybe_result

{-
  where
    pat_id = DbHelpers.db_id db_pattern
-}


-- | Although isomorphic to Maybe, this is necessary to force
-- strict evaluation of the Maybe content so that the
-- timeout works properly.
data MatchAnswer = NoMatch | HasMatch !ScanPatterns.ScanMatch


convertMatchAnswerToMaybe = \case
  HasMatch y -> Just y
  NoMatch    -> Nothing


applySinglePattern ::
     (Int, LT.Text)
  -> ScanPatterns.DbPattern
  -> MatchAnswer
applySinglePattern (line_number, line) db_pattern =
  case match_span of
    Nothing -> NoMatch
    Just x  -> HasMatch $ match_partial x
  where
    pattern_obj = DbHelpers.record db_pattern

    match_span = case ScanPatterns.expression pattern_obj of
      ScanPatterns.RegularExpression regex_text _ -> do
        (match_offset, match_length) <- LT.unpack line =~~ encodeUtf8 regex_text :: Maybe (MatchOffset, MatchLength)
        return $ DbHelpers.StartEnd match_offset (match_offset + match_length)
      ScanPatterns.LiteralExpression literal_text -> do
        first_index <- Safe.headMay (Search.indices literal_text $ LT.toStrict line)
        return $ DbHelpers.StartEnd first_index (first_index + T.length literal_text)

    match_partial = ScanPatterns.NewScanMatch db_pattern .
      ScanPatterns.NewMatchDetails
        line
        line_number


getFirstMatchGroup :: LT.Text -> T.Text -> Maybe String
getFirstMatchGroup extracted_chunk regex_text = do
  (_before, match_array, _after) <- maybe_match
  return $ fst $ match_array ! 1
  where
    maybe_match = (LT.unpack extracted_chunk =~~ encodeUtf8 regex_text) :: Maybe (String, MatchText String, String)
