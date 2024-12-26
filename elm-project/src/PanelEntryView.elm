module PanelEntryView exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Button as Button
import Models exposing (..)


viewPanelEntry : PanelEntry -> Html Msg
viewPanelEntry panelEntry =
    div [ class "panel-entry" ]
        [ div [ id "expression" ] [ text panelEntry.expression ]
        , div [ id "panel-entry-variables" ]
            [ showVariableList panelEntry
            ]
        , Button.text (Button.config |> Button.setOnClick (DeleteExpression panelEntry.expression)) "Delete"
        ]


showSymbolTableEntry : PanelEntry -> SymbolTableEnry -> Html Msg
showSymbolTableEntry panelEntry symbolTableEntry =
    tr []
        [ td [] [ text symbolTableEntry.variable ]
        , td [] [ text (String.fromFloat symbolTableEntry.variableValue) ]
        , td []
            [ div []
                [ input [ onInput (UpdateVarValueBuffer panelEntry symbolTableEntry), value symbolTableEntry.textInput ] []
                , button [] [ text "Update" ]
                ]
            ]
        , td [] [ symbolTableEntry.errMsg |> Maybe.withDefault "" |> text ]
        ]


showVariableList : PanelEntry -> Html Msg
showVariableList panelEntry =
    let
        variables =
            Dict.values panelEntry.variables
    in
    div [ id "variables" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Variable" ]
                    , th [] [ text "Value" ]
                    , th [] [ text "Value Update" ]
                    , th [] [ text "Errors" ]
                    ]
                ]
            , tbody [] (variables |> List.map (showSymbolTableEntry panelEntry))
            ]
        ]
