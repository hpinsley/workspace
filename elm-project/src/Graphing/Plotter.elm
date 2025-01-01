module Graphing.Plotter exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import PanelEntryView exposing (displayPlotValues)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
                    panelEntry.evaluatedPlotValues |> get_ordered_pairs
              in
              case Dict.size panelEntry.variables of
                1 ->
                    plot2d model orderedPairs

                2 ->
                    plot3d model orderedPairs

                _ ->
                    div
                        []
                        [ Html.text "Cannot plot more than 2 variables" ]
            ]
        ]


plot2d : Model -> List (List Float) -> Html Msg
plot2d model orderedPairs =
    div
        [ Html.Attributes.id "plot-2d" ]
        [ Html.text "2D Plot"
        , div []
            [ svg
                [ Svg.Attributes.width "120"
                , Svg.Attributes.height "120"
                , viewBox "0 0 120 120"
                ]
                [ rect
                    [ x "10"
                    , y "10"
                    , Svg.Attributes.width "100"
                    , Svg.Attributes.height "100"
                    , rx "15"
                    , ry "15"
                    ]
                    []
                , circle
                    [ cx "50"
                    , cy "50"
                    , r "50"
                    ]
                    []
                ]
            ]
        ]


plot3d : Model -> List (List Float) -> Html Msg
plot3d model orderedPairs =
    div
        [ Html.Attributes.id "plot-3d" ]
        [ Html.text "3D Plot"
        , div [] [ Debug.toString orderedPairs |> Html.text ]
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
