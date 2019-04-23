{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Maybe                    as Maybe
import qualified Data.Text.Lazy                as L (Text)
import           System.Environment            (lookupEnv)
import qualified Text.Blaze.Html.Renderer.Text as BRT
import qualified Text.Blaze.Html5              as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S
import Control.Monad.IO.Class (liftIO)


import qualified SqlRead


getHtml :: H.Html
getHtml =
  H.span "hello"


main :: IO ()
main = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe 3000 $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  S.scotty prt $ do

    S.post "/start-scan" $ do
      S.html $ BRT.renderHtml getHtml

    S.get "/list-builds" $ do

      builds_list <- liftIO SqlRead.query_builds
      S.json builds_list

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file "./static/index.html"

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file "./static/favicon.ico"
