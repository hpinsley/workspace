module Parsing.ExpressionParsers exposing (..)

import Char
import Parser exposing (..)
import Parsing.ExpressionModels exposing (..)
import Set


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
    case run singleLineExpressionParser expression of
        Ok value ->
            Ok value

        Err deadEnds ->
            deadEnds |> extractParserErrors |> Err


numberParser : Parser Factor
numberParser =
    number
        { int = Just IntFactor
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just FloatFactor
        }


variableParser : Parser Factor
variableParser =
    succeed VariableFactor
        |= variable
            { start = Char.isAlphaNum
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList []
            }


function1Parser : Parser Function1
function1Parser =
    Parser.oneOf
        [ succeed Sin
            |. symbol "sin"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        , succeed Cos
            |. symbol "cos"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        , succeed Tan
            |. symbol "tan"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        ]


unaryfactorParser : Parser Factor
unaryfactorParser =
    oneOf
        [ numberParser |> Parser.backtrackable
        , succeed SingleArgumentFunction
            |= function1Parser
            |> Parser.backtrackable
        , variableParser
        ]


mulOpParser : Parser MulOp
mulOpParser =
    Parser.oneOf
        [ succeed Times |. symbol "*"
        , succeed Divide |. symbol "/"
        ]


addOpParser : Parser AddOp
addOpParser =
    Parser.oneOf
        [ succeed Plus |. symbol "+"
        , succeed Minus |. symbol "-"
        ]


factorParser : Parser Factor
factorParser =
    Parser.oneOf
        [ succeed BinaryFactor
            |= unaryfactorParser
            |= mulOpParser
            |= lazy (\_ -> factorParser)
            |> backtrackable
        , unaryfactorParser
        ]


expressionParser : Parser Expression
expressionParser =
    Parser.oneOf
        [ succeed BinaryExpression
            |= factorParser
            |= addOpParser
            |= lazy (\_ -> expressionParser)
            |> backtrackable
        , succeed UnaryExpression
            |= factorParser
        ]


singleLineExpressionParser : Parser Expression
singleLineExpressionParser =
    expressionParser
        |. end
