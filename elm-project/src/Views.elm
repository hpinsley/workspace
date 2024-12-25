module Views exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Models exposing (..)
import PanelView exposing (viewPanel)
import Time
import Material.Tab
import Material.Button as Button

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


view : Model -> Html Msg
view model =
    div
        [ id "computations" ]
        [ h1 [] [ text "Computations" ]
        , label [] [ text "Expression:" ]
        , input [ id "expression-input", onInput UpdateExpression, value (Maybe.withDefault "" model.expression) ] []
        , Button.text (Button.config |> Button.setOnClick AddToPanel |> Button.setDisabled (isValidExpression model |> not))
        "Add to Panel"
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , viewPanel model
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]
