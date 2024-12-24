module State exposing (..)

import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Task
import Time exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick currentTime ->
            ( tickModel model currentTime, Cmd.none )

        UpdateExpression expr ->
            ( { model | expression = Just expr }, Cmd.none )

        ParseExpression ->
            parseModelExpression model


tickModel : Model -> Time.Posix -> Model
tickModel model theTime =
    { model | currentTime = Just theTime }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- 1every 1000.0 Tick


parseModelExpression : Model -> ( Model, Cmd Msg )
parseModelExpression model =
    case model.expression of
        Nothing ->
            ( { model | parseErrors = "Please enter an expression." }, Cmd.none )

        Just expression ->
            let
                parsedExpression =
                    ExpressionParsers.parseExpression expression
            in
            case parsedExpression of
                Ok goodExpression ->
                    ( { model | parsedExpression = Just goodExpression, parseErrors = "" }, Cmd.none )

                Err errmsg ->
                    ( { model | parseErrors = errmsg }, Cmd.none )


