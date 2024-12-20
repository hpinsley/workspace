module Main exposing (..)

import Browser
import Html exposing (..)
import Models exposing (..)
import Views
import State

-- main : Program () Model Msg
-- main =
--     Html.program
--         { init = Models.init
--         , view = Views.view
--         , update = State.update
--         }

main : Program () Model Msg
main =
    Browser.element
        { init = Models.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = Views.view
        }
