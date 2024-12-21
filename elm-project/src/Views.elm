module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Models exposing (..)

centeredStyle : Attribute msg
centeredStyle =
    style
        [ ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("height", "100vh")
        ]


-- view : Model -> Html Msg
-- view model =
--     div []
--         [ button [ onClick Increment ] [ text "Increment" ]
--         , div [] [ text (String.fromInt model.count) ]
--         ]


view : Model -> Html Msg
view model =
    div [ centeredStyle ]
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        ]
