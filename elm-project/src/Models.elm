module Models exposing (..)

import Time

type Msg
    = Increment
    | Decrement
    | Tick Time.Posix


type alias Model =
    { count : Int
    , currentTime: Maybe Time.Posix
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { count = 0, currentTime = Nothing }, Cmd.none )
