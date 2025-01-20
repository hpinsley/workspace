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


viewPanelEntry : Model -> Int -> PanelEntry -> Html Msg
viewPanelEntry model index panelEntry =
    div
        [ class "panel-entry"
        , class
            (if Utils.isActivePlotPanel model panelEntry then "active" else "inactive")
        ]
        [ div [ id "expression" ] [ text panelEntry.expression ]

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
        , displayAxisInfo model panelEntry index
        , displayRotationSpeed model
        , displayViewportScaling panelEntry index
        ]

displayRotationSpeed: Model -> Html Msg
displayRotationSpeed model =
    fieldset [id "rotation-speed"]
        [   
              legend [][text "Rotation Speed"] 
            , text "Milliseconds:"
            , model.rotationSpeed |> Utils.roundFloat 2 |> String.fromFloat |> text
            , button [onClick IncreaseRotationSpeed] [text "Faster"]
            , button [onClick DecreaseRotationSpeed] [text "Slower"]
            , text " Rotations:"
            , model.rotations |> Utils.roundFloat 2 |> String.fromFloat |> text
            , button [onClick SmootherRotations] [text "Smoother"]
            , button [onClick JumpierRotations] [text "Jumpier"]

        ]


displayViewportScaling : PanelEntry -> Int -> Html Msg
displayViewportScaling panelEntry index =
    div [ id "viewport-scaling" ]
        [ panelEntryAlignmentView index panelEntry.alignmentX (SetXAlignment panelEntry) "X"
        , panelEntryAlignmentView index panelEntry.alignmentY (SetYAlignment panelEntry) "Y"
        , panelEntryAlignmentBehaviorView panelEntry index (SetAlignmentBehavior panelEntry)
        ]

displayAxisInfo : Model -> PanelEntry -> Int -> Html Msg
displayAxisInfo model panelEntry index =
    div [ id "axes-info" ]
        [ 
            fieldset []
                [ legend [] [ text "Axis Rotation" ]
                , panelEntrySingleAxisView model panelEntry panelEntry.xAxis (IncrementXAxisRotation panelEntry) (DecrementXAxisRotation panelEntry) (SetXAxisRotationValue panelEntry)
                , panelEntrySingleAxisView model panelEntry panelEntry.yAxis (IncrementYAxisRotation panelEntry) (DecrementYAxisRotation panelEntry) (SetYAxisRotationValue panelEntry)
                , panelEntrySingleAxisView model panelEntry panelEntry.zAxis (IncrementZAxisRotation panelEntry) (DecrementZAxisRotation panelEntry) (SetZAxisRotationValue panelEntry)

                , fieldset [id "auto-rotate-fieldset"] [
                                legend [] [ text "Auto Rotation Setting" ]
                                , div [] [ createAutoRotateRadioButton panelEntry index "X" RotateX ]
                                , div [] [ createAutoRotateRadioButton panelEntry index "Y" RotateY ]
                                , div [] [ createAutoRotateRadioButton panelEntry index "Z" RotateZ ]
                                , div [] [ createAutoRotateRadioButton panelEntry index "None" NoAutoRotate ]
                   ]
                ]
        ]

createAutoRotateRadioButton : PanelEntry -> Int -> String -> AutoRotate -> Html Msg
createAutoRotateRadioButton panelEntry index axisLetter autoRotate =
    let
        elementId = "set-" ++ axisLetter ++ "-rotate"
    in
        label [][
                    input
                        [ Html.Attributes.id elementId
                        , Html.Attributes.type_ "radio"
                        , Html.Attributes.name ("auto-rotate_" ++ (String.fromInt index))
                        -- , Html.Attributes.value axisLetter
                        , Html.Attributes.checked (panelEntry.autoRotate == autoRotate)
                        , Html.Events.onClick (UpdatePanelEntryAutoRotate panelEntry autoRotate)
                        ]
                        []
                    , text axisLetter
        ]

panelEntrySingleAxisView : Model -> PanelEntry -> Axis -> Msg -> Msg -> (String -> Msg) -> Html Msg
panelEntrySingleAxisView model panelEntry axis incrementMessage decrementMessage rangeValueChangeMessage  =
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
                            , input 
                                [
                                    type_ "range"
                                    , value (axis.rotationAngle |> String.fromFloat)
                                    , disabled (panelEntry.autoRotate /= NoAutoRotate)
                                    , Html.Attributes.min "0"
                                    , 2*pi |> String.fromFloat |> Html.Attributes.max
                                    , model |> Utils.getRotationIncrement |> String.fromFloat |> Html.Attributes.step
                                    , Html.Events.onInput rangeValueChangeMessage
                                ]
                                [
                                ]
                        ]
                ]
        ]

panelEntryAlignmentBehaviorView : PanelEntry -> Int -> (SvgAlignmentBehavor -> Msg) -> Html Msg
panelEntryAlignmentBehaviorView panelEntry index msgFunc =
    fieldset []
        [ legend [] [ text "Behavior" ]
        , div []
            [ input
                [ Html.Attributes.id "alignment-behavior-meet"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name ("alignment-behavior_" ++ (String.fromInt index))
                , Html.Attributes.value "Meet"
                , Html.Attributes.checked (panelEntry.meetOrSlice == Meet)
                , Html.Events.onClick (msgFunc Meet)
                ]
                []
            , label [ Html.Attributes.for "alignment-behavior-meet" ] [ text "Meet" ]
            ]
        , div []
            [ input
                [ Html.Attributes.id "alignment-behavior-slice"
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name ("alignment-behavior_" ++ (String.fromInt index))
                , Html.Attributes.value "Slice"
                , Html.Attributes.checked (panelEntry.meetOrSlice == Slice)
                , Html.Events.onClick (msgFunc Slice)
                ]
                []
            , label [ Html.Attributes.for "alignment-behavior-slice" ] [ text "Slice" ]
            ]
        ]


panelEntryAlignmentView : Int -> SvgAlignment -> (SvgAlignment -> Msg) -> String -> Html Msg
panelEntryAlignmentView index currentAlignmentValue msgFunc axis =
    fieldset []
        [ legend [] [ text (axis ++ " Alignment") ]
        , div []
            [ input
                [ Html.Attributes.id (axis ++ "-align-min")
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name (axis ++ "-alignment_" ++ (String.fromInt index))
                , Html.Attributes.value "Min"
                , Html.Attributes.checked (currentAlignmentValue == AlignMin)
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
                , Html.Attributes.name (axis ++ "-alignment_" ++ (String.fromInt index))
                , Html.Attributes.value "Mid"
                , Html.Attributes.checked (currentAlignmentValue == AlignMid)
                , Html.Events.onClick (msgFunc AlignMid)
                ]
                []
            , label [ Html.Attributes.for "x-align-mid" ] [ text "Mid" ]
            ]
        , div []
            [ input
                [ Html.Attributes.id (axis ++ "-align-max")
                , Html.Attributes.type_ "radio"
                , Html.Attributes.name (axis ++ "-alignment_" ++ (String.fromInt index))
                , Html.Attributes.value "Max"
                , Html.Attributes.checked (currentAlignmentValue == AlignMax)
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
