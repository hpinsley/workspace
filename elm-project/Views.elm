module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Models exposing (Model)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "Increment" ]
        , div [] [ text (String.fromInt model.count) ]
        ]
