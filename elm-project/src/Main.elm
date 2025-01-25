module Main exposing (..)

-- Bump this number to force hot recompile
-- Bump 21

import Browser
import Html exposing (..)
import Json.Decode as Decode
import Models exposing (..)
import State
import Views


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = Views.view
        }
