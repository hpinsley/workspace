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
    | BinaryFactor MulOp Factor Factor


type Expression
    = BinaryExpression Factor AddOp Factor
    | Factor


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing, expression = Nothing, parsedExpression = Nothing }
    in
    ( inital_model, Cmd.none )
