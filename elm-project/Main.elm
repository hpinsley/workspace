module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Models exposing (Model, init, increment)
import Views exposing (view)

type Msg
    = Increment

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            increment model

main : Program () Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        }
