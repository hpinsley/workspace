module Graphing.Plot2d exposing (plot2d)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (Matrix)
import Models exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils


x_TICK_WIDTH_YRANGE_PCT =
    0.04


xTICK_LABEL_OFFSET_HEIGHT_PCT =
    -2.0


y_TICK_WIDTH_XRANGE_PCT =
    0.02


yTICK_LABEL_OFFSET_WIDTH_PCT =
    -2.0


plot2d : Model -> PanelEntry -> List TwoDLineSegment -> Html Msg
plot2d model panelEntry lineSegments =
    let
        _ =
            Debug.log "Plot2D number of segments to plot" (List.length lineSegments)
        _ = Debug.log "Segments to plot" lineSegments

        (v1Points, v2Points) = lineSegments 
                                    |> List.map (\(LineSeg2D from to) -> (from, to))
                                    |> List.unzip

        -- (v1Points, v2Points) = lineSegments |> List.unzip
        allPoints = List.append v1Points v2Points |> Debug.log "all points"

        minX =
            List.minimum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0
        maxX =
            List.maximum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0

        minY =
            List.minimum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0

        maxY =
            List.maximum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0
        xWidth =
            maxX - minX |> Debug.log "xWidth"

        yWidth =
            maxY - minY |> Debug.log "yWidth"

        viewboxAttribte =
            (minX |> String.fromFloat)
                ++ " "
                ++ (minY |> String.fromFloat)
                ++ " "
                ++ (xWidth |> String.fromFloat)
                ++ " "
                ++ (yWidth |> String.fromFloat)
                |> Debug.log "viewboxAttribte"

        yTransform =
            adjustYValue maxY minY

        functionPath =
            build2DPathFromLineSegments yTransform lineSegments |> Debug.log "Function path"

        ( xAxisPath, xLabels ) =
            buildXAxisPath minX maxX minY maxY yTransform |> Debug.log "xAxisPath"

        ( yAxisPath, yLabels ) =
            buildYAxisPath minX maxX minY maxY yTransform |> Debug.log "yAxisPath"

        elements =
            [ Svg.path
                [ Svg.Attributes.d functionPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "0.01"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d xAxisPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "green"
                , Svg.Attributes.strokeWidth "0.01"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d yAxisPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "green"
                , Svg.Attributes.strokeWidth "0.01"
                ]
                []
            ]
                ++ yLabels
                ++ xLabels
    in
    div
        [ Html.Attributes.id "plot-2d" ]
        [ Html.text "2D Plot"
        , div [ Html.Attributes.class "svg-container" ]
            [ svg
                [ Svg.Attributes.width "100%"
                , Svg.Attributes.height "100%"
                , viewBox viewboxAttribte

                -- , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                , Svg.Attributes.preserveAspectRatio (buildPreserveAspectRatioString panelEntry |> Debug.log "preserveAspectRatio")
                ]
                elements
            ]
        ]


buildPreserveAspectRatioString : PanelEntry -> String
buildPreserveAspectRatioString panelEntry =
    let
        xPart =
            case panelEntry.alignmentX of
                AlignMin ->
                    "xMin"

                AlignMid ->
                    "xMid"

                AlignMax ->
                    "xMax"

        -- These are Pascal case
        yPart =
            case panelEntry.alignmentY of
                AlignMin ->
                    "YMin"

                AlignMid ->
                    "YMid"

                AlignMax ->
                    "YMax"

        meetOrSlice =
            case panelEntry.meetOrSlice of
                Meet ->
                    "meet"

                Slice ->
                    "slice"
    in
    xPart ++ yPart ++ " " ++ meetOrSlice


buildXAxisPath : Float -> Float -> Float -> Float -> (Float -> Float) -> ( String, List (Svg Msg) )
buildXAxisPath minX maxX minY maxY yTransform =
    let
        points =
            [ LineSeg2D (Vec2D minX 0.0) (Vec2D maxX 0.0)  ] |> Debug.log "x-axis-points"

        axisLine =
            build2DPathFromLineSegments yTransform points

        ( tickMarks, labels ) =
            buildXAxisTickMarks minX maxX minY maxY yTransform |> Debug.log "x-axis ticks"
    in
    ( axisLine ++ tickMarks, labels )


buildYAxisPath : Float -> Float -> Float -> Float -> (Float -> Float) -> ( String, List (Svg Msg) )
buildYAxisPath minX maxX minY maxY yTransform =
    let
        points =
            [ LineSeg2D (Vec2D 0.0 minY) (Vec2D 0.0 maxY)  ] |> Debug.log "y-axis-points"

        axisLine =
            build2DPathFromLineSegments yTransform points

        ( tickMarks, labels ) =
            buildYAxisTickMarks minX maxX minY maxY yTransform |> Debug.log "y-axis ticks"
    in
    ( axisLine ++ tickMarks, labels )


buildYAxisTickMarks : Float -> Float -> Float -> Float -> (Float -> Float) -> ( String, List (Svg Msg) )
buildYAxisTickMarks xMin xMax yMin yMax yTransform =
    let
        bottomTick =
            floor yMin |> Debug.log "bottom-tick"

        topTick =
            ceiling yMax |> Debug.log "top-tick"

        tickMarksAt =
            List.range bottomTick topTick
                |> List.filter (\i -> i /= 0)
                |> List.map toFloat
                |> Debug.log "tick-marks"

        width =
            abs (xMax - xMin) * y_TICK_WIDTH_XRANGE_PCT |> Debug.log "tick-width"

        xTickStart =
            -width

        xTickEnd =
            width

        tickPoints =
            tickMarksAt
                |> List.map (\yVal -> ( ( xTickStart, yTransform yVal ), ( xTickEnd, yTransform yVal ) ))
                |> Debug.log "tick-points"

        tickCmds =
            tickPoints
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) ->
                        " M "
                            ++ String.fromFloat x1
                            ++ ","
                            ++ String.fromFloat y1
                            ++ " L "
                            ++ String.fromFloat x2
                            ++ ","
                            ++ String.fromFloat y2
                    )
                |> List.foldl (++) ""
                |> Debug.log "cmds"

        labelSvg =
            tickMarksAt
                |> List.map (\y -> ( y, yTransform y, yTICK_LABEL_OFFSET_WIDTH_PCT * width ))
                |> List.map
                    (\( y, yLoc, xLoc ) ->
                        Svg.text_
                            [ Svg.Attributes.x (String.fromFloat xLoc)
                            , Svg.Attributes.y (String.fromFloat yLoc)
                            , Svg.Attributes.fontSize "0.1"
                            , Svg.Attributes.alignmentBaseline "middle"
                            ]
                            [ Svg.text (String.fromFloat y)
                            ]
                    )

        -- tickDistance = (toFloat height) / (toFloat numTicks) |> Debug.log "tick-distance"
    in
    ( tickCmds, labelSvg )


buildXAxisTickMarks : Float -> Float -> Float -> Float -> (Float -> Float) -> ( String, List (Svg Msg) )
buildXAxisTickMarks xMin xMax yMin yMax yTransform =
    let
        leftTick =
            floor xMin |> Debug.log "left-tick"

        rightTick =
            ceiling xMax |> Debug.log "right-tick"

        tickMarksAt =
            List.range leftTick rightTick
                |> List.filter (\i -> i /= 0)
                |> List.map toFloat
                |> Debug.log "x-axis tick-marks"

        height =
            abs (yMax - yMin) * x_TICK_WIDTH_YRANGE_PCT |> Debug.log "x-tick-height"

        yTickStart =
            -height

        yTickEnd =
            height

        tickPoints =
            tickMarksAt
                |> List.map (\xVal -> ( ( xVal, yTransform yTickStart ), ( xVal, yTransform yTickEnd ) ))
                |> Debug.log "x-axis tick-points"

        tickCmds =
            tickPoints
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) ->
                        " M "
                            ++ String.fromFloat x1
                            ++ ","
                            ++ String.fromFloat y1
                            ++ " L "
                            ++ String.fromFloat x2
                            ++ ","
                            ++ String.fromFloat y2
                    )
                |> List.foldl (++) ""
                |> Debug.log "cmds"

        labelSvg =
            tickMarksAt
                |> List.map (\x -> ( x, x, yTransform (xTICK_LABEL_OFFSET_HEIGHT_PCT * height) ))
                |> List.map
                    (\( x, xLoc, yLoc ) ->
                        Svg.text_
                            [ Svg.Attributes.x (String.fromFloat xLoc)
                            , Svg.Attributes.y (String.fromFloat yLoc)
                            , Svg.Attributes.fontSize "0.1"
                            , Svg.Attributes.alignmentBaseline "middle"
                            ]
                            [ Svg.text (String.fromFloat x)
                            ]
                    )

        -- tickDistance = (toFloat height) / (toFloat numTicks) |> Debug.log "tick-distance"
    in
    ( tickCmds, labelSvg )


adjustYValue : Float -> Float -> Float -> Float
adjustYValue maxY minY y =
    (maxY + minY) - y

build2DPathFromLineSegments : (Float -> Float) -> List TwoDLineSegment -> String
build2DPathFromLineSegments yAdjust lineSegments =
    lineSegments 
        |> List.map (build2DPathFromLineSegment yAdjust)
        |> String.join " "
        -- |> Debug.log "2D Path"

build2DPathFromLineSegment : (Float -> Float) -> TwoDLineSegment -> String
build2DPathFromLineSegment yAdjust lineSegment =
    let
        (LineSeg2D from to) = lineSegment
        (Vec2D xFrom yFrom) = from
        (Vec2D xTo yTo) = to
    in
        "M " ++ String.fromFloat xFrom ++ "," ++ String.fromFloat (yAdjust yFrom) ++
            " " ++ 
        "L" ++ String.fromFloat xTo ++ "," ++ String.fromFloat (yAdjust yTo)
