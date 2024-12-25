module PanelEntryView exposing (..)
import Models exposing (PanelEntry, Msg)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


viewPanelEntry : PanelEntry -> Html Msg
viewPanelEntry panelEntry =
    div [class "panel-entry"] [
        div [id "expression"] [text panelEntry.expression]
    ]

