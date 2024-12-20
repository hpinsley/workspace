module Models exposing (..)

type Msg
    = Increment
    
type alias Model =
    { count : Int
    }

init : flags -> (Model, Cmd Msg)
init _ =
    ({ count = 0 }, Cmd.none)

increment : Model -> Model
increment model =
    { model | count = model.count + 1 }
