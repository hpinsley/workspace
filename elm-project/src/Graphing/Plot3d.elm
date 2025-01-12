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


plot3d : Model -> PanelEntry -> List LineSegment -> Html Msg
plot3d model panelEntry lineSegments =
    let
        _ =
            Debug.log "Plot3D points to plot" (List.length lineSegments)

        _ =
            Debug.log "Line Segments" lineSegments

        -- rotatedPairs =
        --     rotateData panelEntry lineSegments

        -- projection =
        --     rotatedPairs
        --         |> List.map Utils.dropYComponent
    in
    div
        [ Html.Attributes.id "plot-3d" ]
        [ 
            -- div [] [ plotProjectedPoints model panelEntry projection ]
        ]


rotateData : PanelEntry -> List Vector -> List Vector
rotateData panelEntry vectors =
    let
        -- rotationMatrix = Utils.xyzRotation (pi/2) 0 0    -- Good for looking at the grid?
        rotationMatrix =
            Utils.xyzRotation panelEntry.xAxis.rotationAngle panelEntry.yAxis.rotationAngle panelEntry.zAxis.rotationAngle
    in
        vectors |> Utils.multiply3DData rotationMatrix


plotProjectedPoints : Model -> PanelEntry -> List LineSegment -> Html Msg
plotProjectedPoints model panelEntry lineSegments =
    let
        _ =
            Debug.log "Plot2D points to plot" (List.length lineSegments)

        (v1Points, v2Points) = lineSegments |> List.unzip
        allPoints = List.append v1Points v2Points |> Debug.log "all points"

        minX =
            List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) allPoints) |> Maybe.withDefault 0.0 |> Debug.log "minX"

        maxX =
            List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) allPoints) |> Maybe.withDefault 0.0 |> Debug.log "maxX"

        minY =
            List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) allPoints) |> Maybe.withDefault 0.0 |> Debug.log "minY"

        maxY =
            List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) allPoints) |> Maybe.withDefault 0.0 |> Debug.log "maxY"

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
            build2DPathFromLineSegments yTransform lineSegments |> Debug.log "Function Path"

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


build2DPathFromLineSegments : (Float -> Float) -> List LineSegment -> String
build2DPathFromLineSegments yAdjust lineSegments =
    lineSegments 
        |> List.map (build2DPathFromLineSegment yAdjust)
        |> String.join " "
        |> Debug.log "2D Path"

build2DPathFromLineSegment : (Float -> Float) -> LineSegment -> String
build2DPathFromLineSegment yAdjust lineSegment =
    let
        (from, to) = lineSegment
        (xFrom, yFrom) = case from of
                            x :: y :: [] -> (x, yAdjust y)
                            _ -> (0,0) |> Debug.log "Unexpected vector length"
        (xTo, yTo) = case to of
                            x :: y :: [] -> (x, yAdjust y)
                            _ -> (0,0) |> Debug.log "Unexpected vector length"
    in
        "M " ++ String.fromFloat xFrom ++ "," ++ String.fromFloat yFrom ++
            " " ++ 
        "L" ++ String.fromFloat xTo ++ "," ++ String.fromFloat yTo
