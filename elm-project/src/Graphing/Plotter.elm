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
                    panelEntry.evaluatedPlotValues |> get_ordered_pairs |> Debug.log "orderedPairs"
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

        yTransform = adjustYValue maxY minY
        functionPath = build2DPath yTransform orderedPairs
        xAxisPath = buildXAxisPath minX maxX yTransform |> Debug.log "xAxisPath"
        yAxisPath = buildYAxisPath minX maxX minY maxY yTransform |> Debug.log "yAxisPath"
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
                            [ Svg.Attributes.d functionPath
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke "black"
                            , Svg.Attributes.strokeWidth "0.01"
                            ]
                            []
                        ,Svg.path
                            [ Svg.Attributes.d xAxisPath
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke "green"
                            , Svg.Attributes.strokeWidth "0.01"
                            ]
                            []
                        ,Svg.path
                            [ Svg.Attributes.d yAxisPath
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke "green"
                            , Svg.Attributes.strokeWidth "0.01"
                            ]
                            []

                    ]
                ]
            ]

buildXAxisPath : Float -> Float -> (Float -> Float) -> String
buildXAxisPath minX maxX yTrasform =
    let
        points = [ [ minX, 0.0 ], [ maxX, 0.0 ] ] |> Debug.log "x-axis-points"
    in
        build2DPath yTrasform points

buildYAxisPath : Float -> Float -> Float -> Float -> (Float -> Float) -> String
buildYAxisPath minX maxX minY maxY yTrasform =
    let
        points = [ [ 0.0, minY ], [ 0.0, maxY ] ] |> Debug.log "y-axis-points"
        tickMarks = buildYAxisTickMarks minX maxX minY maxY |> Debug.log "y-axis ticks"
        axisLine = build2DPath yTrasform points
        ticks = tickMarks
    in
        axisLine ++ tickMarks

buildYAxisTickMarks: Float -> Float -> Float -> Float -> String
buildYAxisTickMarks xMin xMax yMin yMax =
    let
        bottomTick = floor yMin |> Debug.log "bottom-tick"
        topTick = ceiling yMax |> Debug.log "top-tick"

        height = abs(topTick - bottomTick) |> Debug.log "height"
        numTicks = height
        tickDistance = 1

        tickMarksAt = List.range bottomTick topTick
                        |> List.map toFloat |> Debug.log "tick-marks"
        width = abs (xMax - xMin) * 0.05 |> Debug.log "tick-width"
        xTickStart = -width
        xTickEnd = width

        tickPoints = tickMarksAt
            |> List.map (\yVal -> ((xTickStart, yVal), (xTickEnd, yVal))) |> Debug.log "tick-points"

        cmds = tickPoints
            |> List.map (\((x1, y1), (x2, y2)) -> (
                " M " ++ String.fromFloat x1 ++ "," ++ String.fromFloat y1 ++ 
                " L " ++ String.fromFloat x2 ++ "," ++ String.fromFloat y2 
                ))
                |> List.foldl (++) ""
                |> Debug.log "cmds"

        -- tickDistance = (toFloat height) / (toFloat numTicks) |> Debug.log "tick-distance"
    in
        cmds

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
