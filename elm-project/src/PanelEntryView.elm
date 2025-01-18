module PanelEntryView exposing (..)

import Debug exposing (toString)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra
import Material.Button as Button
import Material.Checkbox as Checkbox
import Models exposing (..)
import Utils


viewPanelEntry : Model -> PanelEntry -> Html Msg
viewPanelEntry model panelEntry =
    div
        [ class "panel-entry"
        , class
            (if Utils.isActivePlotPanel model panelEntry then "active" else "inactive")
        ]
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
        , div [ id "panel-errors" ] [ getPanelEntryErrors panelEntry |> text ]
        , Button.text (Button.config |> Button.setOnClick (EvaluateExpression panelEntry.expression)) "Evaluate"
        , Button.text (Button.config |> Button.setOnClick (DeleteExpression panelEntry.expression)) "Delete"
        , div [ id "evaluation" ] [ panelEntry.evaluation |> Maybe.map String.fromFloat |> Maybe.withDefault "" |> text ]
        , Button.text (Button.config |> Button.setOnClick (Plot panelEntry)) "Plot"
        , displayAxisInfo panelEntry
        , displayRotationSpeed model
        , displayViewportScaling panelEntry
        ]

displayRotationSpeed: Model -> Html Msg
displayRotationSpeed model =
    div 
        [id "rotation-speed"]
        [
            fieldset 
                []
                [
                      legend [] [ text "Rotation Speed"]
                    , model.rotationSpeed |> Utils.roundFloat 4 |> String.fromFloat |> (++) "Current: " |> text
                ]
        ]


displayViewportScaling : PanelEntry -> Html Msg
displayViewportScaling panelEntry =
    div [ id "viewport-scaling" ]
        [ panelEntryAlignmentView (SetXAlignment panelEntry) "X"
        , panelEntryAlignmentView (SetYAlignment panelEntry) "Y"
        , panelEntryAlignmentBehaviorView (SetAlignmentBehavior panelEntry)
        ]

displayAxisInfo : PanelEntry -> Html Msg
displayAxisInfo panelEntry =
    div [ id "axes-info" ]
        [ 
            fieldset []
                [ legend [] [ text "Axis Rotation" ]
                , panelEntrySingleAxisView panelEntry.xAxis (IncrementXAxisRotation panelEntry) (DecrementXAxisRotation panelEntry)
                , panelEntrySingleAxisView panelEntry.yAxis (IncrementYAxisRotation panelEntry) (DecrementYAxisRotation panelEntry)
                , panelEntrySingleAxisView panelEntry.zAxis (IncrementZAxisRotation panelEntry) (DecrementZAxisRotation panelEntry)

                , fieldset [id "auto-rotate-fieldset"] [
                                legend [] [ text "Auto Rotation Setting" ]
                                , div [] [ createAutoRotateRadioButton panelEntry "X" RotateX ]
                                , div [] [ createAutoRotateRadioButton panelEntry "Y" RotateY ]
                                , div [] [ createAutoRotateRadioButton panelEntry "Z" RotateZ ]
                                , div [] [ createAutoRotateRadioButton panelEntry "None" NoAutoRotate ]
                   ]
                ]
        ]

createAutoRotateRadioButton : PanelEntry -> String -> AutoRotate -> Html Msg
createAutoRotateRadioButton panelEntry axisLetter autoRotate =
    let
        elementId = "set-" ++ axisLetter ++ "-rotate"
    in
        label [][
                    input
                        [ Html.Attributes.id elementId
                        , Html.Attributes.type_ "radio"
                        , Html.Attributes.name "auto-rotate"
                        -- , Html.Attributes.value axisLetter
                        , Html.Attributes.selected False
                        , Html.Events.onClick (UpdatePanelEntryAutoRotate panelEntry autoRotate)
                        ]
                        []
                    , text axisLetter
        ]

panelEntrySingleAxisView : Axis -> Msg -> Msg -> Html Msg
panelEntrySingleAxisView axis incrementMessage decrementMessage =
    div [ class "axis-info" ]
        [ 
            div [
                    class "min-max-increment"
                ]
                [
                    fieldset []
                        [
                            legend [] [ text axis.axisName ]
                            , axis.rotationAngle |> Utils.roundFloat 2 |> String.fromFloat |> text
                            , button    [
                                              class "inc-button inc-up"
                                            , onClick incrementMessage
                                        ]
                                    [text "+"]
                            , button [
                                              class "inc-button inc-down"
                                            , onClick decrementMessage
                                    ]
                                    [text "-"]
                        ]
                ]
        ]

panelEntryAlignmentBehaviorView : (SvgAlignmentBehavor -> Msg) -> Html Msg
panelEntryAlignmentBehaviorView msgFunc =
    fieldset []
        [ legend [] [ text "Behavior" ]
        , div []
            [ input
                [ Html.Attributes.id "alignment-behavior-meet"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name "alignment-behavior"
                , Html.Attributes.value "Meet"
                , Html.Attributes.selected False
                , Html.Events.onClick (msgFunc Meet)
                ]
                []
            , label [ Html.Attributes.for "alignment-behavior-meet" ] [ text "Meet" ]
            ]
        , div []
            [ input
                [ Html.Attributes.id "alignment-behavior-slice"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name "alignment-behavior"
                , Html.Attributes.value "Slice"
                , Html.Attributes.selected False
                , Html.Events.onClick (msgFunc Slice)
                ]
                []
            , label [ Html.Attributes.for "alignment-behavior-slice" ] [ text "Slice" ]
            ]
        ]


panelEntryAlignmentView : (SvgAlignment -> Msg) -> String -> Html Msg
panelEntryAlignmentView msgFunc axis =
    fieldset []
        [ legend [] [ text (axis ++ " Alignment") ]
        , div []
            [ input
                [ Html.Attributes.id (axis ++ "-align-min")
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name (axis ++ "-alignment")
                , Html.Attributes.value "Min"
                , Html.Attributes.selected False
                , Html.Events.onClick (msgFunc AlignMin)
                ]
                []
            , label [ Html.Attributes.for "x-align-min" ] [ text "Min" ]

            -- , Html.Events.Extra.onChange (SetXAlignment panelEntry)][]
            ]
        , div []
            [ input
                [ Html.Attributes.id (axis ++ "-align-mid")
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name (axis ++ "-alignment")
                , Html.Attributes.value "Mid"
                , Html.Attributes.selected True
                , Html.Events.onClick (msgFunc AlignMid)
                ]
                []
            , label [ Html.Attributes.for "x-align-mid" ] [ text "Mid" ]
            ]
        , div []
            [ input
                [ Html.Attributes.id (axis ++ "-align-max")
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name (axis ++ "-alignment")
                , Html.Attributes.value "Max"
                , Html.Attributes.selected False
                , Html.Events.onClick (msgFunc AlignMax)
                ]
                []
            , label [ Html.Attributes.for "x-align-max" ] [ text "Max" ]

            -- , Html.Events.Extra.onChange (SetXAlignment panelEntry)][]
            ]
        ]



-- Select.filled
--         (Select.config
--             |> Select.setLabel (Just "X Alignment")
--             |> Select.setSelected (Just AlignMid)
--             |> Select.setOnChange (SetXAlignment panelEntry)
--         )
--         (SelectItem.selectItem
--             (SelectItem.config { value = AlignMid })
--             "Mid"
--         )
--         [
--             SelectItem.selectItem
--                 (SelectItem.config { value = AlignMin })
--                 "Min"
--             , SelectItem.selectItem
--                 (SelectItem.config { value = AlignMid })
--                 "Mid"
--             , SelectItem.selectItem
--                 (SelectItem.config { value = AlignMax })
--                 "Max"
--         ]
-- ]


getPanelEntryErrors : PanelEntry -> String
getPanelEntryErrors panelEntry =
    let
        errors =
            panelEntry.variables
                |> Dict.values
                |> List.filterMap .errMsg
                |> String.join ", "
    in
    Maybe.withDefault "" panelEntry.panelError ++ errors


showSymbolTableEntry : PanelEntry -> SymbolTableEntry -> Html Msg
showSymbolTableEntry panelEntry symbolTableEntry =
    tr []
        [ td [ class "variable-name" ] [ text symbolTableEntry.variable ]
        , td []
            [ div []
                [ symbolTableEntry.currentValue
                    |> Utils.roundFloat 3
                    |> String.fromFloat
                    |> text
                ]
            ]
        , td []
            [ div []
                [ input
                    [ class "var-input"
                    , Html.Events.Extra.onChange (UpdateVarStartValue panelEntry symbolTableEntry)
                    , onInput (UpdateVarStartValueBuffer panelEntry symbolTableEntry)
                    , value symbolTableEntry.startValueBuffer
                    ]
                    []
                ]
            ]
        , td []
            [ div []
                [ if symbolTableEntry.mayVary then
                    input
                        [ class "var-input"
                        , Html.Events.Extra.onChange (UpdateVarEndValue panelEntry symbolTableEntry)
                        , onInput (UpdateVarEndValueBuffer panelEntry symbolTableEntry)
                        , value symbolTableEntry.endValueBuffer
                        ]
                        []

                  else
                    text ""
                ]
            ]
        , td []
            [ div []
                [ if symbolTableEntry.mayVary then
                    input
                        [ class "var-input"
                        , Html.Events.Extra.onChange (UpdateVarIncrementValue panelEntry symbolTableEntry)
                        , onInput (UpdateVarIncrementValueBuffer panelEntry symbolTableEntry)
                        , value symbolTableEntry.incrementValueBuffer
                        ]
                        []

                  else
                    text ""
                ]
            ]
        , td []
            [ div []
                [ input
                    [ type_ "checkbox"
                    , Html.Attributes.checked symbolTableEntry.mayVary
                    , onClick (ToggleVarMayVary panelEntry symbolTableEntry)
                    ]
                    []
                ]
            ]
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
                        , th [] [ text "Vary" ]
                        ]
                    ]
                , tbody [] (variables |> List.map (showSymbolTableEntry panelEntry))
                ]
            ]
