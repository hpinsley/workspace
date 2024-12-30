module PanelView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import PanelEntryView exposing (viewPanelEntry)
import Material.Button as Button


viewPanel : Model -> Html Msg
viewPanel model =
    div [ id "panel" ]
        [
            div [ id "panel-entries"] (model.panelEntries |> List.map viewPanelEntry)
            , Button.text (Button.config |> Button.setOnClick Plot) "Plot"

        ]
