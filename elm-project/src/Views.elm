module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Models exposing (..)
import Time
import Dict

getFormattedTime : Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing ->
            "No time"

        Just posixTime ->
            Iso8601.fromTime posixTime

showVariableList : Model -> Html Msg
showVariableList model =
    let
        variables = Dict.values model.variables
        trs = variables |> List.map (\v -> tr [] [td [][text v]])
    in
        div [id "variables"][
            table [][
                thead [][
                    tr [][
                        th [][text "Variable"]
                    ]
                ]
                , tbody [] trs
            ]
        ]

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
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , showVariableList model
        ]
