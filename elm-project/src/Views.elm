module Views exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Material.Button as Button
import Material.Tab
import Material.TextField as TextField
import Models exposing (..)
import PanelView exposing (viewPanel)
import Time
import Graphing.Plotter exposing (plot)


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
        [ label [] [ text "Expression" ]
        , input [ id "expression-input", title "Expression", onInput UpdateExpression, value (Maybe.withDefault "" model.expression) ] []
        , Button.text (Button.config |> Button.setOnClick AddToPanel |> Button.setDisabled (isValidExpression model |> not)) "Add to Panel"
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , viewPanel model
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]


rightSide : Model -> Html Msg
rightSide model =
    div [ id "right-side" ]
        [ h1 [] [ case model.activePlotEntry of
                    Just activePanelEntry ->
                        plot model activePanelEntry

                    Nothing ->
                        text "No active expression"
                ]
        ]


view : Model -> Html Msg
view model =
    div [ id "screen" ]
        [ leftSide model
        , rightSide model
        ]
