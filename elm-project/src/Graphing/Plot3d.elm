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
        -- value_matrix = Matrix.fromLists orderedPairs
        y_radacted = orderedPairs
            |> List.map (\v -> List.take 2 v)
        -- theta = pi / 4.0

        -- _ = Matrix.pretty (\v -> Debug.toString v) (Utils.x3dRotation theta) |> Debug.log "xmatrix"
        -- _ = Matrix.pretty (\v -> Debug.toString v) (Utils.y3dRotation theta) |> Debug.log "ymatrix"
        -- _ = Matrix.pretty (\v -> Debug.toString v) (Utils.z3dRotation theta) |> Debug.log "zmatrix"

    in
        div
            [ Html.Attributes.id "plot-3d" ]
            [ Html.text "3D Plot"
            , div [] [ plot2d model panelEntry y_radacted ]
            ]

