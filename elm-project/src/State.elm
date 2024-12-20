module State exposing (..)
import Models exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            (increment model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    
