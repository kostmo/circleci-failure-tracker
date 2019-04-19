{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Maybe                    as Maybe
import qualified Data.Text.Lazy                as L (Text)
import qualified Diagrams.Backend.SVG          as Svg
import           Diagrams.Prelude              as DP
import qualified Graphics.Svg.Core             as SC
import           System.Environment            (lookupEnv)
import qualified Text.Blaze.Html.Renderer.Text as BRT
import qualified Text.Blaze.Html5              as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Read                     (readMaybe)
import qualified Web.Scotty                    as S

import qualified Floorplan
import qualified Measurements
import qualified Rectangles
import qualified Render
import qualified TileGenerator


-- | XXX Note that the return type of the 'renderDia' function cannot be
-- declared, because 'Graphics.Rendering.SVG.SvgM' is not exported publicly!
getRawSvgMarkup ::
     Double
  -> Diagram Svg.B
  -> L.Text
getRawSvgMarkup width diagram =
  SC.renderText $ renderDia Svg.SVG svg_options diagram
  where
    svg_options = Svg.SVGOptions (mkWidth width) Nothing "" [] False


getHtml :: TileGenerator.TilingConfig -> Floorplan.Floorplan -> H.Html
getHtml tiling_config (Floorplan.NewFloorplan page_title floorplan_points) =

  H.span H.! A.id "network-diagram-container" $ H.preEscapedToHtml raw_svg_text

  where
    raw_svg_text = getRawSvgMarkup 600 my_diagram

    (_, _, my_diagram) = Render.renderSingleDiagram
      "basic"
      floorplan_points
      tiling_config
      0


getStaticConfig :: Floorplan.WebNumbers -> TileGenerator.StaticTilingConfig
getStaticConfig web_numbers = TileGenerator.NewStaticTilingConfig
      (realToFrac $ Floorplan.groutWidth web_numbers)
      rect_size
    where
      rect_size = Rectangles.NewRectSize
        (realToFrac $ Floorplan.tileWidth web_numbers)
        (realToFrac $ Floorplan.tileHeight web_numbers)


prepareConfig web_input_floorplan_config =
  tiling_config
  where
    static_config = getStaticConfig web_input_floorplan_config

    manually_specified_offsets = TileGenerator.ManuallySpecified $
      TileGenerator.NewManuallySpecifiedOffsets
        (-Measurements.wallIrregularityMargin)
        (-Measurements.wallIrregularityMargin)
        (realToFrac $ Floorplan.adjacentOffsetFraction web_input_floorplan_config)

    tiling_config = TileGenerator.NewTilingConfig
      static_config
      manually_specified_offsets


main :: IO ()
main = do

  maybe_envar_port <- lookupEnv "PORT"
  let prt = Maybe.fromMaybe 3000 $ readMaybe =<< maybe_envar_port
  putStrLn $ "Listening on port " <> show prt

  S.scotty prt $ do

    S.post "/floorplan" $ do
      web_inputs <- S.jsonData :: S.ActionM Floorplan.WebInputs
      let tiling_config = prepareConfig $ Floorplan.config web_inputs
      S.html $ BRT.renderHtml $ getHtml tiling_config $ Floorplan.floorplan web_inputs

    S.post "/floorplan-default" $ do

      web_numbers <- S.jsonData :: S.ActionM Floorplan.WebNumbers
      let tiling_config = prepareConfig web_numbers

      S.html $ BRT.renderHtml $ getHtml
        tiling_config
        (Floorplan.NewFloorplan "default" Measurements.targPtsSimple)

    S.get "/download-sample" $
      S.json $ Floorplan.NewFloorplan "sample" Measurements.targPts

    S.options "/" $ do
      S.setHeader "Access-Control-Allow-Origin" "*"
      S.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"

    S.get "/" $ do
      S.setHeader "Content-Type" "text/html; charset=utf-8"
      S.file "./static/index.html"

    S.get "/favicon.ico" $ do
      S.setHeader "Content-Type" "image/x-icon"
      S.file "./static/favicon.ico"
