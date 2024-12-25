module PanelView exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)
import PanelEntryView exposing (viewPanelEntry)

viewPanel : Model -> Html Msg
viewPanel model =
    div [id "panel"] 
        (model.panelEntries |> List.map viewPanelEntry)
    
