module Main exposing (..)

import Browser
import Html exposing (..)
import Models exposing (..)
import Views
import State

centeredStyle : Attribute msg
centeredStyle =
    style
        [ ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("height", "100vh")
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = Models.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = Views.view
        }

view : Model -> Html Msg
view model =
    div [ centeredStyle ]
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        ]
