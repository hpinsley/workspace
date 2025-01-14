module Graphing.Plot3d exposing (plot3d)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Utils


strokeWidth =
    0.006


plot3d : Model -> PanelEntry -> List ThreeDLineSegment -> Html Msg
plot3d model panelEntry lineSegments =
    let
        _ =
            Debug.log "Plot3D points to plot" (List.length lineSegments)

        -- _ =
        --     Debug.log "Ordered Pairs" lineSegments

        rotatedPairs =
             rotateData panelEntry lineSegments -- |> Debug.log "Rotated pairs"

        projection =
             rotatedPairs
                 |> List.map (\(LineSeg3D from to) -> 
                                let
                                    projectedFrom = Utils.dropYFrom3DVector from
                                    projectedTo = Utils.dropYFrom3DVector to
                                in
                                    LineSeg2D projectedFrom projectedTo
                            )
            -- |> Debug.log "Projection"
    in
    div
        [ Html.Attributes.id "plot-3d" ]
        [ 
            div [] [ plotProjectedPoints model panelEntry projection ]
        ]


rotateData : PanelEntry -> List ThreeDLineSegment -> List ThreeDLineSegment
rotateData panelEntry lineSegments =
    let
        rotationMatrix =
            Utils.xyzRotation panelEntry.xAxis.rotationAngle panelEntry.yAxis.rotationAngle panelEntry.zAxis.rotationAngle

        fromVectors = lineSegments |> List.map (\(LineSeg3D from _) -> from)
        toVectors = lineSegments |> List.map (\(LineSeg3D _ to) -> to)
        
        rotatedFromVectors = fromVectors |> Utils.multiply3DData rotationMatrix
        rotatedToVectors = toVectors |> Utils.multiply3DData rotationMatrix

        rotatedLineSegments = List.map2 (\vfrom vTo -> LineSeg3D vfrom vTo) rotatedFromVectors rotatedToVectors
    in
        rotatedLineSegments


plotProjectedPoints : Model -> PanelEntry -> List TwoDLineSegment -> Html Msg
plotProjectedPoints model panelEntry lineSegments =
    let
        -- _ =
        --     Debug.log "Plot2D points to plot" (List.length lineSegments)

        (v1Points, v2Points) = lineSegments 
                                    |> List.map (\(LineSeg2D from to) -> (from, to))
                                    |> List.unzip
        allPoints = List.append v1Points v2Points -- |> Debug.log "all points"

        minX =
            List.minimum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0
        maxX =
            List.maximum (List.map (\(Vec2D x _) -> x) allPoints) |> Maybe.withDefault 0.0

        minY =
            List.minimum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0

        maxY =
            List.minimum (List.map (\(Vec2D _ y) -> y) allPoints) |> Maybe.withDefault 0.0
        xWidth =
            maxX - minX |> Debug.log "xWidth"

        yWidth =
            maxY - minY |> Debug.log "yWidth"

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
                |> Debug.log "viewboxAttribte"

        yTransform =
            adjustYValue maxY minY

        functionPath =
            build2DPathFromLineSegments yTransform lineSegments -- |> Debug.log "Function Path"

        elements =
            [ Svg.path
                [ Svg.Attributes.d functionPath
                , Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
                ]
                []
            ]
    in
    div
        [ Html.Attributes.id "projection" ]
        [ Html.text "Projection"
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


adjustYValue : Float -> Float -> Float -> Float
adjustYValue maxY minY y =
    (maxY + minY) - y


build2DPathFromLineSegments : (Float -> Float) -> List TwoDLineSegment -> String
build2DPathFromLineSegments yAdjust lineSegments =
    lineSegments 
        |> List.map (build2DPathFromLineSegment yAdjust)
        |> String.join " "
        -- |> Debug.log "2D Path"

-- This method takes a list of 2D line segments to plot and adjusts the y component using the
-- given method
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
