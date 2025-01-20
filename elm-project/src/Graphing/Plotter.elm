module Graphing.Plotter exposing (..)

import Dict exposing (..)
import Graphing.Plot2d exposing (..)
import Graphing.Plot3d exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils
import String exposing (lines)
import Color exposing (Color)

plot : Model -> PanelEntry -> Html Msg
plot model panelEntry =
    let
        colorMapper = buildColorMapperFromModel model
    in    
        div
            [ Html.Attributes.id "plot" ]
            [ 
                -- div
                --     [ Html.Attributes.id "plot-header" ]
                --     [ h2 [] [ Html.text panelEntry.expression ]
                --     ], 
                
                div
                [ Html.Attributes.id "plot-body" ]
                [ let
                    lineSegments =
                        panelEntry.evaluatedPlotValues -- |> Debug.log "evaluatedPlotValues"
                in
                case Utils.getVaryingVariableCount panelEntry of
                    1 ->
                        lineSegments
                            |> List.map Utils.generalLineSegmentToLineSegment2D
                            |> plot2d model panelEntry
                    2 ->
                        lineSegments
                            |> List.map Utils.generalLineSegmentToLineSegment3D
                            |> plot3d model panelEntry colorMapper
                        -- div []
                        --     [ Html.text "3-D Plot note ready" ]

                    _ ->
                        div
                            []
                            [ Html.text "Cannot plot more than 2 variables" ]
                ]
            ]

-- In plot3d, we want to vary the color by the yDepth.  In that function, we compute a value
-- (here called colorVaryParam) mapped in the range of 0..1 for the yvalues.  We use that float to vary color.
-- Given the model, we build a function that maps a float to a color

buildColorMapperFromModel : Model -> (Float -> Color)
buildColorMapperFromModel model =
    let
        helperFunc : Int -> Int -> Float -> Int
        helperFunc min max f =
            let
                range = (max - min) |> toFloat
                rgbSingle = (toFloat min) + f * range |> round
            in
                rgbSingle

        mappingFunc : (Float -> Color)
        mappingFunc colorVaryParam =
            let
                shadingRange = model.shadingRange
                r = helperFunc shadingRange.minRed shadingRange.maxRed colorVaryParam
                g = helperFunc shadingRange.minGreen shadingRange.maxGreen colorVaryParam
                b = helperFunc shadingRange.minBlue shadingRange.maxBlue colorVaryParam
            in
                Color.rgb255 r g b
    in
        mappingFunc
