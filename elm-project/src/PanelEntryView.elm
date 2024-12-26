module PanelEntryView exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Button as Button
import Models exposing (..)
import Material.IconToggle as IconToggle
import Material.Checkbox as Checkbox

viewPanelEntry : PanelEntry -> Html Msg
viewPanelEntry panelEntry =
    div [ class "panel-entry" ]
        [ div [ id "expression" ] [ text panelEntry.expression ]
        -- , IconToggle.iconToggle
        --         (IconToggle.config 
        --             |> IconToggle.setOn panelEntry.isCollapsed
        --             |> IconToggle.setOnChange (TogglePanelEntry panelEntry)
        --         )
        --         { offIcon = IconToggle.icon "favorite_border"
        --         , onIcon = IconToggle.icon "favorite"
        --         }
            , Checkbox.checkbox
                (Checkbox.config
                    |> Checkbox.setState (Just (if panelEntry.isCollapsed then Checkbox.checked else Checkbox.unchecked))
                    |> Checkbox.setOnChange (TogglePanelEntry panelEntry)
                )
            , div [ id "panel-entry-variables" ]
            [ showVariableList panelEntry
            ]
        , Button.text (Button.config |> Button.setOnClick (DeleteExpression panelEntry.expression)) "Delete"
        ]


showSymbolTableEntry : PanelEntry -> SymbolTableEntry -> Html Msg
showSymbolTableEntry panelEntry symbolTableEntry =
    tr []
        [ td [] [ text symbolTableEntry.variable ]
        , td [] [ text (String.fromFloat symbolTableEntry.variableValue) ]
        , td []
            [ div []
                [ 
                    input [ onInput (UpdateVarValueBuffer panelEntry symbolTableEntry), value symbolTableEntry.textInput ] []
                , Button.text (Button.config |> Button.setOnClick (UpdateVarValue panelEntry symbolTableEntry)) "Update"

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
        if panelEntry.isCollapsed then
            div [] []
        else
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
