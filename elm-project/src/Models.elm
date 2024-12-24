module Models exposing (..)

import Parsing.ExpressionModels exposing (Expression)
import Time


type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | ParseExpression


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    , parseErrors : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing, expression = Nothing, parsedExpression = Nothing, parseErrors = "" }
    in
    ( inital_model, Cmd.none )
