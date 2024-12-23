module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Models exposing (..)
import Time


getFormattedTime : Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing ->
            "No time"

        Just posixTime ->
            Iso8601.fromTime posixTime


view : Model -> Html Msg
view model =
    div
        [ id "adder" ]
        [ h1 [] [ text "Computations" ]
        , label [] [ text "Expression:" ]
        , input [ onInput UpdateExpression ] []
        , button [ class "command", onClick ParseExpression ] [ text "Parse" ]
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        , div [] [ text (String.fromInt (String.length (Maybe.withDefault "" model.expression))) ]
        ]
