module Graphing.Plot3d exposing (plot3d)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils
import Svg exposing (..)
import Svg.Attributes exposing (..)

strokeWidth = 0.006

plot3d : Model -> PanelEntry -> List Vector -> Html Msg
plot3d model panelEntry orderedPairs =
    let
        _ = Debug.log "Plot3D points to plot" (List.length orderedPairs)
        _ = Debug.log "Ordered Pairs" orderedPairs

        rotatedPairs = rotateData(orderedPairs)

        projection = rotatedPairs
            |> List.map Utils.dropYComponent
    in
        div
            [ Html.Attributes.id "plot-3d" ]
            [ 
                div [] [ plotProjectedPoints model panelEntry projection ]
            ]

rotateData: List Vector -> List Vector
rotateData vectors =
    let
        rotationMatrix = Utils.xyzRotation (pi/2) 0 0
    in
        vectors |> Utils.multiply3DData rotationMatrix
        
plotProjectedPoints : Model -> PanelEntry -> List Vector -> Html Msg
plotProjectedPoints model panelEntry orderedPairs =
    let
        _ = Debug.log "Plot2D points to plot" (List.length orderedPairs)
        minX =
            List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "minX"

        maxX =
            List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "maxX"

        minY =
            List.minimum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "minY"

        maxY =
            List.maximum (List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) orderedPairs) |> Maybe.withDefault 0.0 |> Debug.log "maxY"

        xWidth =
            maxX - minX |> Debug.log "xWidth"

        yWidth =
            maxY - minY |> Debug.log "yWidth"

        reduction = 0.9
        expansion = 1.0 / reduction
        offset = (1.0 - reduction) / 2.0

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
            build2DPath yTransform orderedPairs |> Debug.log "Function Path"

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
        xPart = case panelEntry.alignmentX of
                AlignMin -> "xMin"
                AlignMid -> "xMid"
                AlignMax -> "xMax"

        -- These are Pascal case
        yPart = case panelEntry.alignmentY of
                AlignMin -> "YMin"
                AlignMid -> "YMid"
                AlignMax -> "YMax"

        meetOrSlice = case panelEntry.meetOrSlice of
            Meet -> "meet"
            Slice -> "slice"
    in
        xPart ++ yPart ++ " " ++ meetOrSlice

adjustYValue : Float -> Float -> Float -> Float
adjustYValue maxY minY y =
    (maxY + minY) - y


build2DPath : (Float -> Float) -> List Vector -> String
build2DPath yAdjust orderedPairs =
    let
        xValues =
            List.map (\pair -> Maybe.withDefault 0.0 (List.head pair)) orderedPairs

        yValues =
            List.map (\pair -> Maybe.withDefault 0.0 (List.head (Maybe.withDefault [] (List.tail pair)))) orderedPairs

        adjustedYValues =
            List.map yAdjust yValues

        points =
            List.map2 (\x y -> String.fromFloat x ++ "," ++ String.fromFloat y) xValues adjustedYValues

        path =
            "M " ++ (List.head points |> Maybe.withDefault "0,0") ++ " L " ++ (List.tail points |> Maybe.withDefault [] |> String.join " L ")
    in
        path
