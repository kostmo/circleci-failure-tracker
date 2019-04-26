{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Maybe                    as Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
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


getHtml :: H.Html
getHtml =
  H.span "hello"


gen_pattern_page :: [(Builds.BuildNumber, Text, ScanPatterns.MatchDetails)] -> H.Html
gen_pattern_page myrows = H.html $ do
  H.head $ do
    H.title "Pattern occurrences"
    H.link H.! A.href "/static/style.css" H.! A.rel "stylesheet"

  body

  {-
        return [
            htmlgen.make_link(htmlgen.gen_circleci_build_url(build_num), str(build_num)),
            row[1],
            str(row[2]),
            tag("span", line[0:start])
                + tag("span", line[start:end], {"class": "highlight"})
                + tag("span", line[end:])
        ]
  -}
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

    S.post "/start-scan" $ do
      S.html $ BRT.renderHtml getHtml

    S.get "/pattern-occurrences" $ do
      pattern_id <- S.param "pattern_id"
      rows <- liftIO $ SqlRead.get_pattern_occurrence_rows pattern_id
      let page_content = gen_pattern_page rows
      S.html $ BRT.renderHtml page_content

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

    S.get "/api/patterns" $ do
      builds_list <- liftIO SqlRead.api_patterns
      S.json builds_list

    S.get "/api/pattern-details" $ do
      pattern_id <- S.param "pattern_id"
      rows <- liftIO $ SqlRead.get_pattern_details pattern_id
      S.json rows

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file "./static/index.html"

    S.get "/static/script/mychart.js" $ do
      S.setHeader "Content-Type" "text/javascript; charset=utf-8"
      S.file "./static/script/mychart.js"

    S.get "/static/pattern-details.html" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file "./static/pattern-details.html"

    S.get "/static/style.css" $ do
      S.setHeader "Content-Type" "text/css; charset=utf-8"
      S.file "./static/style.css"

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file "./static/favicon.ico"
