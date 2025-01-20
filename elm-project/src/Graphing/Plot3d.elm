module Graphing.Plot3d exposing (plot3d)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils
import Color exposing (Color)

logEnabled = False

strokeWidth =
    0.006

axisScalar = 1.0

plot3d : Model -> PanelEntry -> (Float -> Color) -> List ThreeDLineSegment -> Html Msg
plot3d model panelEntry colorMapper lineSegments =
    let
        _ =
            log3D "Plot3D points to plot" (List.length lineSegments)

        -- _ =
        --     log3D "Ordered Pairs" lineSegments

        rotationMatrix = Utils.xyzRotation panelEntry.xAxis.rotationAngle panelEntry.yAxis.rotationAngle panelEntry.zAxis.rotationAngle
        rotatedData = rotateData rotationMatrix lineSegments -- |> log3D "Rotated pairs"
        rotatedAxes = rotateAxes rotationMatrix (buildAxes panelEntry lineSegments)
    in
    div
        [ Html.Attributes.id "plot-3d" ]
        [ 
            div [Html.Attributes.id "projected-points"] [ 
                        projectAndPlotPoints model panelEntry rotatedData rotatedAxes colorMapper
                ]
        ]

buildAxes: PanelEntry -> List ThreeDLineSegment -> (ThreeDLineSegment, ThreeDLineSegment, ThreeDLineSegment)
buildAxes panelEntry data =
    let
        xAxis = LineSeg3D (Vec3D -axisScalar 0 0) (Vec3D axisScalar 0 0)
        yAxis = LineSeg3D (Vec3D 0 -axisScalar 0) (Vec3D 0 axisScalar 0)
        zAxis = LineSeg3D (Vec3D 0 0 -axisScalar) (Vec3D 0 0 axisScalar)
    in
        (xAxis, yAxis, zAxis)

rotateData : FloatMatrix -> List ThreeDLineSegment -> List ThreeDLineSegment
rotateData rotationMatrix lineSegments =
    let
        fromVectors = lineSegments |> List.map (\(LineSeg3D from _) -> from)
        toVectors = lineSegments |> List.map (\(LineSeg3D _ to) -> to)
        
        rotatedFromVectors = fromVectors |> Utils.multiply3DData rotationMatrix
        rotatedToVectors = toVectors |> Utils.multiply3DData rotationMatrix

        rotatedLineSegments = List.map2 (\vfrom vTo -> LineSeg3D vfrom vTo) rotatedFromVectors rotatedToVectors
    in
        rotatedLineSegments

rotateAxes : FloatMatrix -> (ThreeDLineSegment, ThreeDLineSegment, ThreeDLineSegment) 
                -> (ThreeDLineSegment, ThreeDLineSegment, ThreeDLineSegment)
rotateAxes rotationMatrix (x, y, z) =
      (
            Utils.multiply3DLineSegment rotationMatrix x
        ,   Utils.multiply3DLineSegment rotationMatrix y
        ,   Utils.multiply3DLineSegment rotationMatrix z
      )

getPercentOfY : List (ThreeDLineSegment) -> List Float
getPercentOfY lineSegments =
    let
        startingPoints = lineSegments |> List.map (\(LineSeg3D from _) -> from)
        startingYValues = startingPoints |> List.map (\(Vec3D _ y _) -> y)
        minYValue = startingYValues |> List.minimum |> Maybe.withDefault 0.0
        maxYValue = startingYValues |> List.maximum |> Maybe.withDefault 0.0
        yRange = maxYValue - minYValue
        pctOfRange = startingYValues |> List.map (\yVal -> (yVal - minYValue) / yRange)
    in
        pctOfRange

projectAndPlotPoints : Model -> PanelEntry -> List ThreeDLineSegment -> (ThreeDLineSegment, ThreeDLineSegment, ThreeDLineSegment) -> (Float -> Color) -> Html Msg
projectAndPlotPoints model panelEntry lineSegments3d (xAxis, yAxis, zAxis) colorMapper =
    let
        computeYPcts = getPercentOfY lineSegments3d -- |> Debug.log "yPcts"
        lineSegmentColors = computeYPcts |> List.map colorMapper

        -- We will be dropping the Y.  We want to adjust color based on yValue before we
        -- drop it


        -- Project down to 2D by dropping the y values
        lineSegments = lineSegments3d |> List.map Utils.dropYFrom3DLineSegment
        coloredLineSegments = List.map2 (\lineSeg c -> ColoredLineSeg2D lineSeg c) lineSegments lineSegmentColors

        projectedXAxis = Utils.dropYFrom3DLineSegment xAxis  
        projectedYAxis = Utils.dropYFrom3DLineSegment yAxis  
        projectedZAxis = Utils.dropYFrom3DLineSegment zAxis  


        (v1Points, v2Points) = lineSegments 
                                    |> List.map (\(LineSeg2D from to) -> (from, to))
                                    |> List.unzip
        allPoints = List.append v1Points v2Points -- |> log3D "all points"

        minX =
            List.minimum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0
        maxX =
            List.maximum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0

        minY =
            List.minimum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0

        maxY =
            List.maximum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0
        xWidth =
            maxX - minX |> log3D "xWidth"

        yWidth =
            maxY - minY |> log3D "yWidth"

        reduction =
            0.9

        expansion =
            1.0 / reduction

        offset =
            (1.0 - reduction) / 2.0

        viewboxAttribte =
            (minX - offset * xWidth |> String.fromFloat)
                ++ " "
                ++ (minY - offset * yWidth |> String.fromFloat)
                ++ " "
                ++ (expansion * xWidth |> String.fromFloat)
                ++ " "
                ++ (expansion * yWidth |> String.fromFloat)
                |> log3D "viewboxAttribte"

        yTransform =
            adjustYValue maxY minY

        svgPathList =
            build2DPathFromLineSegments yTransform coloredLineSegments -- |> log3D "Function Path"

        xAxisPath = buildStrokeFromLineSegment yTransform projectedXAxis
        yAxisPath = buildStrokeFromLineSegment yTransform projectedYAxis
        zAxisPath = buildStrokeFromLineSegment yTransform projectedZAxis

        elements =
            [ Svg.path
                [ Svg.Attributes.d xAxisPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "red"
                , Svg.Attributes.strokeWidth (String.fromFloat (2* strokeWidth))
                ]
                []
            , Svg.path
                [ Svg.Attributes.d yAxisPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "green"
                , Svg.Attributes.strokeWidth (String.fromFloat (2 * strokeWidth))
                ]
                []
            , Svg.path
                [ Svg.Attributes.d zAxisPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "blue"
                , Svg.Attributes.strokeWidth (String.fromFloat (2 * strokeWidth))
                ]
                []
            ]
    in
    div
        [ Html.Attributes.id "projection" ]
        [ 
            div [ Html.Attributes.class "svg-container" ]
                [ svg
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , viewBox viewboxAttribte

                    -- , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                    , Svg.Attributes.preserveAspectRatio (buildPreserveAspectRatioString panelEntry |> log3D "preserveAspectRatio")
                    ]
                    (elements ++ svgPathList)
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


adjustYValue : Float -> Float -> Float -> Float
adjustYValue maxY minY y =
    (maxY + minY) - y


-- This method takes a list of 2D line segments to plot and adjusts the y component using the
-- given method
build2DPathFromLineSegments : (Float -> Float) -> List TwoDColoredLineSegment -> List (Svg Msg)
build2DPathFromLineSegments yAdjust lineSegments =
    lineSegments 
        |> List.map (buildSvgPathPathFromLineSegment yAdjust)

        -- |> log3D "2D Path"

buildStrokeFromLineSegment : (Float -> Float) -> TwoDLineSegment -> String
buildStrokeFromLineSegment yAdjust lineSegment =
    let
        (LineSeg2D from to) = lineSegment
        (Vec2D xFrom yFrom) = from
        (Vec2D xTo yTo) = to
        stroke =  "M " ++ String.fromFloat xFrom ++ "," ++ String.fromFloat (yAdjust yFrom) ++
                    " " ++ "L" ++ String.fromFloat xTo ++ "," ++ String.fromFloat (yAdjust yTo)
    in
        stroke

buildSvgPathPathFromLineSegment : (Float -> Float) -> TwoDColoredLineSegment -> Svg Msg
buildSvgPathPathFromLineSegment yAdjust (ColoredLineSeg2D lineSegment color) =
    let
        stroke = buildStrokeFromLineSegment yAdjust lineSegment
        rgb = Color.toCssString color 
        svgPath =  Svg.path
                        [ Svg.Attributes.d stroke
                        , Svg.Attributes.fill "none"
                        , Svg.Attributes.stroke rgb
                        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                        ]
                        []
    in
        svgPath
log3D : String -> a -> a
log3D msg obj =
    if logEnabled then (Debug.log msg obj) else obj

