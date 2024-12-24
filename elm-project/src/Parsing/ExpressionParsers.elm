module Parsing.ExpressionParsers exposing (..)

import Parsing.ExpressionModels exposing (..)
import Parser exposing (..)
import Char
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
    case run expressionParser expression of
        Ok value ->
            Ok value

        Err deadEnds ->
            deadEnds |> extractParserErrors |> Err


unaryfactorParser : Parser Factor
unaryfactorParser =
    number
        { int = Just IntFactor
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just FloatFactor
        }


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


variableParser : Parser String
variableParser =
  variable
    { start = Char.isAlphaNum
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ ]
    }

function1Parser: Parser Function1
function1Parser =
    Parser.oneOf
        [ 
            succeed Sin 
                |. symbol "sin"
                |. symbol "("
                |= variableParser
                |. symbol ")"
        ]

factorParser : Parser Factor
factorParser =
    Parser.oneOf
        [ succeed BinaryFactor
            |= unaryfactorParser
            |= mulOpParser
            |= lazy (\_ -> factorParser)
            |> backtrackable
        , succeed SingleArgumentFunction
            |= function1Parser
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
