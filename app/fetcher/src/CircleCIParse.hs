{-# LANGUAGE OverloadedStrings #-}

module CircleCIParse where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.ByteString.Internal  (unpackChars)
import           Data.ByteString.Lazy      (ByteString)
import           Data.JsonStream.Parser    (arrayOf, parseLazyByteString, value,
                                            (.:))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT
import qualified Network.HTTP.Client       as HttpClient
import qualified Network.HTTP.Types.Header as HttpHeader
import           Text.Read                 (readMaybe)
import           Text.Regex                (mkRegex, subRegex)
import           Text.Regex.Posix.Wrap     (Regex)

import qualified Builds
import qualified MyUtils
import qualified ScanRecords
import           SillyMonoids              ()
import qualified SqlWrite


-- | Log sizes capped at 100MB
logTruncationSize :: Integer
logTruncationSize = 100 * (10 ^ 6)


-- | Strips bold markup and coloring
ansiRegex :: Text.Regex.Posix.Wrap.Regex
ansiRegex = mkRegex "\x1b\\[([0-9;]*m|K)"


-- | TODO: Consider using attoparsec instead of regex:
-- https://pl-rants.net/posts/regexes-and-combinators/
filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex line ""


-- | Take elements of the list until a size threshold is
-- reached.  Returns True in the output tuple if
-- the intput list was truncated, and False if all elements were accepted.
takeUntilSize ::
     Integer -- ^ truncation size threshold
  -> [LT.Text]
  -> (Bool, [LT.Text])
takeUntilSize = takeUntilSizeRec []


takeUntilSizeRec ::
     [LT.Text] -- ^ previously accumulated elements
  -> Integer -- ^ truncation size threshold
  -> [LT.Text]
  -> (Bool, [LT.Text])
takeUntilSizeRec accepted _ [] = (False, accepted)
takeUntilSizeRec accepted threshold (x:xs) =
  if next_size <= threshold
    then takeUntilSizeRec (accepted ++ [x]) (threshold - next_size) xs
    else (True, accepted)
  where
    next_size = fromIntegral $ LT.length x


-- | Note that we must use incremental JSON parsing here,
-- as the console logs (spread across multiple JSON elements)
-- may be multiple gigabytes.
doParse :: MonadIO m =>
     ScanRecords.ScanCatchupResources
  -> Builds.BuildStepId
  -> HttpClient.Response ByteString
  -> m [LT.Text]
doParse scan_resources build_step_id log_download_result = do

  let maybe_header_size :: Maybe Integer
      maybe_header_size = do
        my_bytestring <- lookup HttpHeader.hContentLength $ HttpClient.responseHeaders log_download_result
        let my_string = unpackChars my_bytestring
        readMaybe my_string

  liftIO $ MyUtils.debugList [
      "Declared JSON response size (possibly compressed):"
    , show maybe_header_size
    ]

  liftIO $ MyUtils.debugStr "Log downloaded."

  let response_body = HttpClient.responseBody log_download_result

  let incremental_parser = arrayOf $ (,)
        <$> "type" .: value
        <*> "message" .: value

      parent_elements = parseLazyByteString incremental_parser response_body :: [(T.Text, LT.Text)]

      -- for some reason the log is sometimes split into sections,
      -- so we concatenate all of the "out" elements.
      -- Empirically, each segment is about 5MB.
      -- Therefore, for a threshold of 1GB, we should only
      -- accept up to 200 segments.
  let pred x = fst x == "out"
      output_elements = map snd $ filter pred parent_elements

      -- Note the double reverse, since we want to take the logs
      -- from the end of the file.
      -- XXX NOTE: Preserving the tail of the log rather than the
      -- head is a user convenience. However, it may actually
      -- negate the memory savings of the incremental JSON parse!
      (was_truncated, raw_console_log) = fmap (mconcat . reverse) $
        takeUntilSize logTruncationSize $ reverse output_elements

      -- This is done line-by-line to save memory
      ansi_stripped_log_lines = map (LT.pack . filterAnsi . LT.unpack) $ LT.lines raw_console_log

      ansi_stripped_log = LT.unlines ansi_stripped_log_lines
      byte_count = LT.length ansi_stripped_log

  liftIO $ do

    MyUtils.debugList [
        "Storing log to database..."
      , "Will be truncated?"
      , show was_truncated
      ]

    MyUtils.debugList [
        "Log size after ANSI stripping:"
      , show byte_count
      , "bytes,"
      , show $ length ansi_stripped_log_lines
      , "lines"
      ]

    SqlWrite.storeLogInfo scan_resources build_step_id $
      ScanRecords.LogInfo
        byte_count
        (length ansi_stripped_log_lines)
        ansi_stripped_log
        (ansi_stripped_log /= raw_console_log)
        was_truncated

    MyUtils.debugStr "Log stored."

  return ansi_stripped_log_lines
