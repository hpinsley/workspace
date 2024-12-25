module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Models exposing (..)
import Time
import Dict
import PanelView exposing (viewPanel)

getFormattedTime : Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing ->
            "No time"

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
        [ id "adder" ]
        [ h1 [] [ text "Computations" ]
        , label [] [ text "Expression:" ]
        , input [ id "expression-input", onInput UpdateExpression, value (Maybe.withDefault "" model.expression) ] []
        , button [ disabled (isValidExpression model |> not), class "command", onClick AddToPanel ] [ text "Add to Panel" ]
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , viewPanel model
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]
