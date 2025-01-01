module PanelView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Button as Button
import Models exposing (..)
import PanelEntryView exposing (viewPanelEntry)


viewPanel : Model -> Html Msg
viewPanel model =
    div [ id "panel" ]
        [ div [ id "panel-entries" ]
            (model.panelEntries |> List.map (viewPanelEntry model))
        ]
