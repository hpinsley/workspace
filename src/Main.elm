module Main exposing (..)

import Browser
import Html exposing (text)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Model =
    { message : String }


init : Model
init =
    { message = "Hello, World!" }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    text model.message
