module PanelEntryView exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (..)
import Material.Button as Button


viewPanelEntry : PanelEntry -> Html Msg
viewPanelEntry panelEntry =
    div [ class "panel-entry" ]
        [ div [ id "expression" ] [ text panelEntry.expression ]
        , div [ id "panel-entry-variables" ]
            [ showVariableList panelEntry
            ]
         , Button.text (Button.config |> Button.setOnClick (DeleteExpression panelEntry.expression)) "Delete"
        ]


showSymbolTableEntry : SymbolTableEnry -> Html Msg
showSymbolTableEntry symbolTableEntry =
    tr [ ]
        [ td [ ] [ text symbolTableEntry.variable ]
        , td [ ] [ text (String.fromFloat symbolTableEntry.variableValue) ]
        ]

showVariableList : PanelEntry -> Html Msg
showVariableList panelEntry =
    let
        variables = Dict.values panelEntry.variables
    in
        div [ id "variables" ]
            [ table []
                [ thead []
                    [ tr []
                        [ th [] [ text "Variable" ]
                        , th [] [ text "Value" ]
                        ]
                    ]
                , tbody [] (variables |> List.map showSymbolTableEntry)
                ]
            ]
