module State exposing (..)

import Models exposing (..)
import Task
import Time exposing (..)
import Parser exposing (..)

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

parseModelExpression : Model -> (Model, Cmd Msg )
parseModelExpression model =
    case model.expression of
        Nothing -> ( { model | parseErrors = "Please enter an expression."}, Cmd.none )
        Just expression -> 
            let
                parsedExpression = parseExpression expression
            in
                case parsedExpression of
                    Ok goodExpression ->
                        ( { model | parsedExpression = Just goodExpression, parseErrors = "" }, Cmd.none)
                    Err errmsg ->
                        ( { model | parseErrors = errmsg }, Cmd.none)

problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting " ++ str
        ExpectingInt ->
            "Expecting an integer"
        ExpectingHex ->
            "Expecting a hexadecimal number"
        ExpectingOctal ->
            "Expecting an octal number"
        ExpectingBinary ->
            "Expecting a binary number"
        ExpectingFloat ->
            "Expecting a float"
        ExpectingNumber ->
            "Expecting a number"
        ExpectingVariable ->
            "Expecting a variable"
        ExpectingSymbol str ->
            "Expecting symbol " ++ str
        ExpectingKeyword str ->
            "Expecting keyword " ++ str
        ExpectingEnd ->
            "Expecting end of input"
        UnexpectedChar ->
            "Unexpected character"
        Problem str ->
            "Problem: " ++ str
        BadRepeat ->
            "Bad repeat"

extractParserErrors : List DeadEnd -> String
extractParserErrors deadEnds =
    deadEnds 
        |> List.map (\deadEnd -> deadEnd.problem)
        |> List.map problemToString
        |> String.join "; "

parseExpression : String -> Result String Expression
parseExpression expression =
    -- Debug.log ("Parsing " ++ expression)
    case run expressionParser expression of
        Ok value ->
            Ok value
        Err deadEnds ->
            deadEnds |> extractParserErrors |> Err

unaryfactorParser: Parser Factor
unaryfactorParser =
    number {
        int = Just IntFactor,
        hex = Nothing,
        octal = Nothing,
        binary = Nothing,
        float = Just FloatFactor
    }

mulOpParser: Parser MulOp
mulOpParser =
    Parser.oneOf [
        succeed Times |. symbol "*"
        , succeed Divide |. symbol "/"
    ]

addOpParser: Parser AddOp
addOpParser =
    Parser.oneOf [
        succeed Plus |. symbol "+"
        , succeed Minus |. symbol "-"
    ]

factorParser: Parser Factor
factorParser =
    Parser.oneOf [
        succeed BinaryFactor 
            |= unaryfactorParser 
            |= mulOpParser
            |= unaryfactorParser
            |> backtrackable
        
        , unaryfactorParser
    ]

expressionParser: Parser Expression
expressionParser =
    Parser.oneOf [
        succeed BinaryExpression 
            |= factorParser 
            |= addOpParser
            |= factorParser
            |> backtrackable
        
        , succeed UnaryExpression
            |= factorParser
    ]
