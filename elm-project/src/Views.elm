module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Material.Button as Button
import Models exposing (..)
import PanelView exposing (viewPanel)
import Time
import Utils
import Help exposing (displayHelp)


view : Model -> Html Msg
view model =
    div [ id "screen" ]
        [ leftSide model
        , rightSide model
        ]

getFormattedTime : Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing ->
            ""

        Just posixTime ->
            Iso8601.fromTime posixTime


isValidExpression : Model -> Bool
isValidExpression model =
    case model.parsedExpression of
        Nothing ->
            False

        Just _ ->
            True


leftSide : Model -> Html Msg
leftSide model =
    div
        [ id "left-side" ]
        [ div [id "left-top"] 
            [
                  label [] [ text "Expression" ]
                , input [ id "expression-input", title "Expression", onInput UpdateExpression, value (Maybe.withDefault "" model.expression) ] []
                , Button.text (Button.config |> Button.setOnClick AddToPanel |> Button.setDisabled (isValidExpression model |> not)) "Add to Panel"
                , button [id "help-btn", onClick (ShowHelp True)] [text "?"]
                , div [ id "parseErrors" ] [ text model.parseErrors ]
            ]
        , viewPanel model
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]

rightSide : Model -> Html Msg
rightSide model =
    div
        [ id "right-side"
        ]
        [
            if model.displayHelp
                then 
                    displayHelp model
                else
                    rightSidePlot model            
        ]

rightSidePlot : Model -> Html Msg
rightSidePlot model =
    case Utils.findActivePanelEntry model of
    Just activePanelEntry ->
        activePanelEntry.currentPlot

    Nothing ->
        div []
            [ text "No active plot"
            , br [] []

            -- , displayMouseInfo model
            ]
            


displayMouseInfo : Model -> Html Msg
displayMouseInfo model =
    case model.mouseDownEventInfo of
        Nothing ->
            text "No mouse info"

        Just mdInfo ->
            div []
                [ text "Mouse is down"
                , br [] []
                , text (Debug.toString mdInfo)
                ]
