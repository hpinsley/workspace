module Graphing.Plotter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)
import Models exposing (..)
import PanelEntryView exposing (displayPlotValues)

plot : Model -> PanelEntry -> Html Msg
plot model panelEntry =
    div
        [ id "plot" ]
        [ div
            [ id "plot-header" ]
            [ h2 [] [ text panelEntry.expression ]
            ]
        , div
            [ id "plot-body" ]
            [ 
                let 
                    orderedPairs = panelEntry.evaluatedPlotValues |> get_ordered_pairs
                in
                    case Dict.size panelEntry.variables of
                        1 ->
                            plot2d model orderedPairs
                        2 ->
                            plot3d model orderedPairs
                        _ ->
                            div
                                [  ]
                                [ text "Cannot plot more than 2 variables"]

            ]
        ]

plot2d : Model -> List (List Float) -> Html Msg
plot2d model orderedPairs =
    div
        [ id "plot-2d" ]
        [ 
            text "2D Plot"
            , div [][Debug.toString orderedPairs |> text]
        ]

plot3d : Model -> List (List Float) -> Html Msg
plot3d model orderedPairs =
    div
        [ id "plot-3d" ]
        [ 
            text "3D Plot"
          , div [][Debug.toString orderedPairs |> text]
        ]


get_ordered_pairs: List ( Dict.Dict String Float, Result String Float ) -> List(List Float)
get_ordered_pairs plotValues =
    plotValues
            |> List.map (\(dict, result) -> case result of 
                                            Ok value -> Just (dict, value) 
                                            Err _ -> Nothing)
            |> List.filterMap identity
            |> List.map (\(dict, value) -> List.append (Dict.values dict)  [value])
