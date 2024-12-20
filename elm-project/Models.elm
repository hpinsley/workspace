module Models exposing (..)

type alias Model =
    { count : Int
    }

init : Model
init =
    { count = 0
    }

increment : Model -> Model
increment model =
    { model | count = model.count + 1 }
