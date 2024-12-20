module State exposing (..)
import Models exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            (increment model, Cmd.none)

increment : Model -> Model
increment model =
    { model | count = model.count + 1 }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    
