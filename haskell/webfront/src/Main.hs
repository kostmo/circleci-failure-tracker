{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Maybe                    as Maybe
import           Data.Text                     (Text)
import           Network.Wai.Middleware.Static
import           System.Environment            (lookupEnv)
import qualified Text.Blaze.Html.Renderer.Text as BRT
import qualified Text.Blaze.Html5              as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S

import qualified Builds
import qualified HtmlUtils
import qualified ScanPatterns
import qualified SqlRead
import qualified WebApi


getHtml :: H.Html
getHtml =
  H.span "hello"


-- | XXX Not used
gen_pattern_page :: [(Builds.BuildNumber, Text, ScanPatterns.MatchDetails)] -> H.Html
gen_pattern_page myrows = H.html $ do
  H.head $ do
    H.title "Pattern occurrences"
    H.link H.! A.href "/static/style.css" H.! A.rel "stylesheet"

  body

  where

    format_row :: (Builds.BuildNumber, Text, ScanPatterns.MatchDetails) -> [H.Html]
    format_row (Builds.NewBuildNumber buildnum, stepname, ScanPatterns.NewMatchDetails line_text line_number (ScanPatterns.NewMatchSpan _start _end)) = [
        H.a H.! A.href (H.toValue $ "https://circleci.com/gh/pytorch/pytorch/" <> show buildnum) $ H.toMarkup $ show buildnum
      , H.toMarkup stepname
      , H.toMarkup $ show line_number
      , H.toMarkup line_text
      ]

    headings = [
        "Build number",
        "Build step",
        "Line number",
        "Line text"
      ]

    body = H.body $ do
      H.h2 "Occurrences of a pattern"
      -- H.p $ "pattern id: " ++ show pattern_id
      HtmlUtils.make_table headings $ map format_row myrows


main :: IO ()
main = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe 3000 $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  S.scotty prt $ do

    S.middleware $ staticPolicy (noDots >-> addBase "static")

    S.post "/start-scan" $ do
      S.html $ BRT.renderHtml getHtml

    -- XXX Not used
    S.get "/pattern-occurrences" $ do
      pattern_id <- S.param "pattern_id"
      rows <- liftIO $ SqlRead.get_pattern_occurrence_rows pattern_id
      let page_content = gen_pattern_page rows
      S.html $ BRT.renderHtml page_content

    -- XXX Not used
    S.get "/list-builds" $ do
      builds_list <- liftIO SqlRead.query_builds
      S.json builds_list

    S.get "/api/failed-commits-by-day" $ do
      builds_list <- liftIO SqlRead.api_failed_commits_by_day
      S.json builds_list

    S.get "/api/job" $ do
      builds_list <- liftIO SqlRead.api_jobs
      S.json builds_list

    S.get "/api/step" $ do
      builds_list <- liftIO SqlRead.api_step
      S.json builds_list

    S.get "/api/summary" $ do
      stats <- liftIO SqlRead.api_summary_stats
      S.json stats

    S.get "/api/disk" $ do
      stats <- liftIO WebApi.api_disk_space
      S.json stats

    S.get "/api/pattern" $ do
      pattern_id <- S.param "pattern_id"
      patterns_list <- liftIO $ SqlRead.api_single_pattern $ read pattern_id
      S.json patterns_list

    S.get "/api/patterns" $ do
      patterns_list <- liftIO $ SqlRead.api_patterns
      S.json patterns_list

    S.get "/api/pattern-matches" $ do
      pattern_id <- S.param "pattern_id"
      rows <- liftIO $ SqlRead.get_pattern_matches pattern_id
      S.json rows

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file "./static/index.html"

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file "./static/favicon.ico"
