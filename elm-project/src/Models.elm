module Models exposing (..)

import Time

type Msg
    = Increment
    | Decrement
    | Tick Time.Posix
    | UpdateExpression String



type alias Model =
    { count : Int
    , currentTime: Maybe Time.Posix
    , expression: Maybe String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { count = 0, currentTime = Nothing, expression = Nothing }, Cmd.none )
