module Graphing.Plot3d exposing (plot3d)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Utils
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Matrix exposing (Matrix)
import Graphing.Plot2d exposing (..)


plot3d : Model -> PanelEntry -> List (List Float) -> Html Msg
plot3d model panelEntry orderedPairs =
    let
        m = Matrix.fromLists orderedPairs |> Maybe.withDefault (Matrix.identity 3)
        _ = m |> Utils.printMatrix "Original Matrix "
        rotatedPairs = rotateData(orderedPairs)

        y_radacted = rotatedPairs
            |> List.map Utils.dropYComponent
    in
        div
            [ Html.Attributes.id "plot-3d" ]
            [ Html.text "3D Plot"
            , div [] [ plot2d model panelEntry y_radacted ]
            ]

rotateData: List(List Float) -> List(List Float)
rotateData inputData =
    let
        rotationMatrix = Utils.y3dRotation(1.2)
    in
        inputData |> Utils.multiply3DData rotationMatrix
        
