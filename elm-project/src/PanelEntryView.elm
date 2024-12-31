module PanelEntryView exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra
import Material.Button as Button
import Material.Checkbox as Checkbox
import Models exposing (..)
import Parser exposing (symbol)
import Debug exposing (toString)


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
                |> Checkbox.setState
                    (Just
                        (if panelEntry.isCollapsed then
                            Checkbox.checked

                         else
                            Checkbox.unchecked
                        )
                    )
                |> Checkbox.setOnChange (TogglePanelEntry panelEntry)
            )
        , div [ id "panel-entry-variables" ]
            [ showVariableList panelEntry
            ]
        , Button.text (Button.config |> Button.setOnClick (EvaluateExpression panelEntry.expression)) "Evaluate"
        , Button.text (Button.config |> Button.setOnClick (DeleteExpression panelEntry.expression)) "Delete"
        , div [ id "evaluation" ] [ panelEntry.evaluation |> Maybe.map String.fromFloat |> Maybe.withDefault "" |> text ]
        , div [ id "plot-values"] [ displayPlotValues panelEntry ]
        ]

displayPlotValues : PanelEntry -> Html Msg
displayPlotValues panelEntry =
    div [] [
        div [] [ "Value Count: " ++ (panelEntry.plotValues |> List.length |> toString) |> text]
        , case panelEntry.plotValues of
            [] ->
                div [] [ text "No plot values" ]

            head :: tail ->
                case tail of 
                    [] ->
                        div [] [ showPlotValue head ]
                    _ -> case List.reverse tail |> List.head of
                        Just lastEntry ->
                            div [] [ showPlotValue head, showPlotValue lastEntry ]
                        Nothing ->
                            div [] [ showPlotValue head ]
        ]
showPlotValue : Dict.Dict String Float -> Html Msg
showPlotValue plotValue =
    div [] [toString plotValue |> text]

showSymbolTableEntry : PanelEntry -> SymbolTableEntry -> Html Msg
showSymbolTableEntry panelEntry symbolTableEntry =
    tr []
        [ td [] [ text symbolTableEntry.variable ]
        , td []
            [ div []
                [ symbolTableEntry.currentValue |> String.fromFloat |> text ]
            ]
                    , td []
            [ div []
                [ input
                    [ Html.Events.Extra.onChange (UpdateVarStartValue panelEntry symbolTableEntry)
                    , onInput (UpdateVarStartValueBuffer panelEntry symbolTableEntry)
                    , value symbolTableEntry.startValueBuffer
                    ]
                    []
                ]
            ]

        , td []
            [ div []
                [ input
                    [ Html.Events.Extra.onChange (UpdateVarEndValue panelEntry symbolTableEntry)
                    , onInput (UpdateVarEndValueBuffer panelEntry symbolTableEntry)
                    , value symbolTableEntry.endValueBuffer
                    ]
                    []
                ]
            ]
        , td []
            [ div []
                [ input
                    [ Html.Events.Extra.onChange (UpdateVarIncrementValue panelEntry symbolTableEntry)
                    , onInput (UpdateVarIncrementValueBuffer panelEntry symbolTableEntry)
                    , value symbolTableEntry.incrementValueBuffer
                    ]
                    []
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
                        , th [] [ text "Start" ]
                        , th [] [ text "End" ]
                        , th [] [ text "Increment" ]
                        , th [] [ text "Errors" ]
                        ]
                    ]
                , tbody [] (variables |> List.map (showSymbolTableEntry panelEntry))
                ]
            ]
