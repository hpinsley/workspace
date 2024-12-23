module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Models exposing (..)
import Iso8601


-- centeredStyle : Attribute msg
-- centeredStyle =
--     style "color" "red"
--         [ ("display", "flex")
--         , ("justify-content", "center")
--         , ("align-items", "center")
--         , ("height", "100vh")
--         ]
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ button [ onClick Increment ] [ text "Increment" ]
--         , div [] [ text (String.fromInt model.count) ]
--         ]

getFormattedTime: Maybe Time.Posix -> String
getFormattedTime timeInfo =
    case timeInfo of
        Nothing -> "No time"
        Just posixTime -> Iso8601.fromTime posixTime

view : Model -> Html Msg
view model =
    div
        [ id "adder" ]
        [ h1 [] [text "Computations"] ,
        label [] [text "Expression"],
        input [] [],
        button [ class "command"] [ text "Update Expression"],
                button [ 
              id "plus-button"
            , onClick Increment             
            , class "command"
        ] [ text "+" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [id "time"][
            getFormattedTime model.currentTime |> text
        ]
        ]
