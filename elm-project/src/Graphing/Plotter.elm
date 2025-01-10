module Graphing.Plotter exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Graphing.Plot2d exposing (..)
import Graphing.Plot3d exposing (..)

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
                orderedPairs =
                    panelEntry.evaluatedPlotValues |> get_ordered_pairs -- |> Debug.log "orderedPairs"
              in
              case Utils.getVaryingVariableCount panelEntry of
                1 ->
                    plot2d model panelEntry orderedPairs

                2 ->
                    plot3d model panelEntry orderedPairs

                _ ->
                    div
                        []
                        [ Html.text "Cannot plot more than 2 variables" ]
            ]
        ]

get_ordered_pairs : List ( Dict.Dict String Float, Result String Float ) -> List (List Float)
get_ordered_pairs plotValues =
    plotValues
        |> List.map
            (\( dict, result ) ->
                case result of
                    Ok value ->
                        Just ( dict, value )

                    Err _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.map (\( dict, value ) -> List.append (Dict.values dict) [ value ])
