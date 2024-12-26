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


showVariableList : PanelEntry -> Html Msg
showVariableList panelEntry =
    let
        variables =
            Dict.values panelEntry.variables

        trs =
            variables |> List.map (\v -> tr [] [ td [ class "variable-name" ] [ text v.variable ], td [] [ v.variableValue |> String.fromFloat |> text ] ])
    in
    div [ id "variables" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Variable" ]
                    , th [] [ text "Value" ]
                    ]
                ]
            , tbody [] trs
            ]
        ]
