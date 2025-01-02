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
    let
        minX = List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "minX"
        maxX = List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "maxX"
        minY = List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "minY"
        maxY = List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "maxY"
        xWidth = maxX - minX |> Debug.log "xWidth"
        yWidth = maxY - minY |> Debug.log "yWidth"
        viewboxAttribte = (minX |> String.fromFloat)
                            ++ " "
                            ++ (minY |> String.fromFloat)
                            ++ " "
                            ++ (xWidth |> String.fromFloat)
                            ++ " "
                            ++ (yWidth |> String.fromFloat)
                            |> Debug.log "viewboxAttribte"
        path = build2DPath (adjustYValue maxY minY) orderedPairs |> Debug.log "path"
    in
        div
            [ Html.Attributes.id "plot-2d" ]
            [ Html.text "2D Plot"
            , div [Html.Attributes.id "svg-container"]
                [ svg
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , viewBox viewboxAttribte
                    ]
                    [ 
                        Svg.path
                            [ Svg.Attributes.d path
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke "black"
                            , Svg.Attributes.strokeWidth "0.01"
                            ]
                            []
                    ]
                ]
            ]

adjustYValue: Float -> Float -> Float -> Float
adjustYValue maxY minY y = 
    (maxY + minY) - y

build2DPath : (Float -> Float) -> List (List Float) -> String
build2DPath yAdjust orderedPairs =
    let
        xValues = List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs
        yValues = List.map (\pair -> (Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair))))) orderedPairs
        adjustedYValues = List.map yAdjust yValues

        points = List.map2 (\x y -> (String.fromFloat x) ++ "," ++ (String.fromFloat y)) xValues adjustedYValues
        path = "M " ++ (List.head points |> Maybe.withDefault "0,0") ++ " L " ++ (List.tail points |> Maybe.withDefault [] |> String.join " L ")
    in
        path

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
