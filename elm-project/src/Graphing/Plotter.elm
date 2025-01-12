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


plot : Model -> PanelEntry -> Html Msg
plot model panelEntry =
    div
        [ Html.Attributes.id "plot" ]
        [ div
            [ Html.Attributes.id "plot-header" ]
            [ h2 [] [ Html.text panelEntry.expression ]
            ]
        , div
            [ Html.Attributes.id "plot-body" ]
            [ let
                lineSegments =
                    panelEntry.evaluatedPlotValues |> Debug.log "evaluatedPlotValues"
              in
              case Utils.getVaryingVariableCount panelEntry of
                1 ->
                    plot2d model panelEntry lineSegments

                2 ->
                    plot3d model panelEntry lineSegments
                    -- div []
                    --     [ Html.text "3-D Plot note ready" ]

                _ ->
                    div
                        []
                        [ Html.text "Cannot plot more than 2 variables" ]
            ]
        ]
