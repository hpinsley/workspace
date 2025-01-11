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


plot3d : Model -> PanelEntry -> List Vector -> Html Msg
plot3d model panelEntry orderedPairs =
    let
        _ = Debug.log "Plot3D points to plot" (List.length orderedPairs)
        
        rotatedPairs = rotateData(orderedPairs)

        y_radacted = rotatedPairs
            |> List.map Utils.dropYComponent
    in
        div
            [ Html.Attributes.id "plot-3d" ]
            [ Html.text "3D Plot"
            , div [] [ plot2d model panelEntry y_radacted ]
            ]

rotateData: List Vector -> List Vector
rotateData vectors =
    let
        rotationMatrix = Utils.y3dRotation(pi / 4.0)
    in
        vectors |> Utils.multiply3DData rotationMatrix
        
