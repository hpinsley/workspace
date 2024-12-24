module Models exposing (..)

import Parsing.ExpressionModels exposing (Expression, Variable)
import Time
import Dict exposing (..)


type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | ParseExpression


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    , parseErrors : String
    , variables: Dict String Variable
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing, expression = Nothing, parsedExpression = Nothing, parseErrors = "", variables = Dict.empty }
    in
    ( inital_model, Cmd.none )
