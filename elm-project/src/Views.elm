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
        , label [] [ text "Expression" ]
        , input [ onInput UpdateExpression ] []
        , button [ class "command" ] [ text "Update Expression" ]
        , button
            [ id "plus-button"
            , onClick Increment
            , class "command"
            ]
            [ text "+" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]
