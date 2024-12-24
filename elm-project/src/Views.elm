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
        trs = variables |> List.map (\v -> tr [] [td [][text v], td [][text "0"]])
    in
        div [id "variables"][
            table [][
                thead [][
                    tr [][
                          th [][text "Variable"]
                        , th [][text "Value"]
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
        , input [ id "expression-input", onInput UpdateExpression ] []
        , button [ class "command", onClick ParseExpression ] [ text "Parse" ]
        , showVariableList model
        , div [ id "parseErrors" ] [ text model.parseErrors ]
        , div [ id "time" ]
            [ getFormattedTime model.currentTime |> text
            ]
        ]
