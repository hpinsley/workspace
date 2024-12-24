module Models exposing (..)

import Time


type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | ParseExpression


type MulOp
    = Times
    | Divide


type AddOp
    = Plus
    | Minus


type Factor
    = IntFactor Int
    | FloatFactor Float
    | BinaryFactor Factor MulOp Factor


type Expression
    = BinaryExpression Factor AddOp Expression
    | UnaryExpression Factor


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    , parseErrors: String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing, expression = Nothing, parsedExpression = Nothing, parseErrors = "" }
    in
    ( inital_model, Cmd.none )
