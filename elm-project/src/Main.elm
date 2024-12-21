module Main exposing (..)

import Browser
import Html exposing (..)
import Models exposing (..)
import State
import Views


main : Program () Model Msg
main =
    Browser.element
        { init = Models.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = Views.view
        }
