module State exposing (..)

import Models exposing (..)
import Task
import Time exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( increment model, Cmd.none )

        Decrement ->
            ( decrement model, Cmd.none )

        Tick currentTime ->
            ( tickModel model currentTime, Cmd.none )

        UpdateExpression expr ->
            ( { model | expression = Just expr }, Cmd.none )


tickModel : Model -> Time.Posix -> Model
tickModel model theTime =
    { model | currentTime = Just theTime }


increment : Model -> Model
increment model =
    { model | count = model.count + 1 }


decrement : Model -> Model
decrement model =
    { model | count = model.count - 1 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- 1every 1000.0 Tick
