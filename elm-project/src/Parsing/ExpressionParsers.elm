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
            , reserved = Set.fromList [ "e", "pi" ]
            }


constantParser : Parser Factor
constantParser =
    Parser.oneOf
        [ succeed (FloatFactor pi)
            |. symbol "pi"
            |> Parser.backtrackable
        , succeed (FloatFactor e)
            |. symbol "e"
        ]


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
        , succeed Abs
            |. symbol "abs"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        , succeed Ln
            |. symbol "ln"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        , succeed Sqrt
            |. symbol "sqrt"
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> Parser.backtrackable
        ]


unSignedUnaryfactorParser : Parser Factor
unSignedUnaryfactorParser =
    oneOf
        [ numberParser |> Parser.backtrackable
        , succeed SingleArgumentFunction
            |= function1Parser
            |> Parser.backtrackable
        , variableParser |> Parser.backtrackable
        , constantParser
        ]


unaryfactorParser : Parser Factor
unaryfactorParser =
    Parser.oneOf
        [ succeed NegatedFactor
            |. symbol "-"
            |= unSignedUnaryfactorParser
            |> backtrackable
        , unSignedUnaryfactorParser
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
        [ succeed Power
            |= unaryfactorParser
            |. symbol "^"
            |= lazy (\_ -> factorParser)
            |> backtrackable
        , succeed BinaryFactor
            |= unaryfactorParser
            |= mulOpParser
            |= lazy (\_ -> factorParser)
            |> backtrackable
        , succeed ExpressionFactor
            |. symbol "("
            |= lazy (\_ -> expressionParser)
            |. symbol ")"
            |> backtrackable
        , unaryfactorParser
        ]


termParser : Parser Term
termParser =
    Parser.oneOf
        [ succeed BinaryTerm
            |= lazy (\_ -> factorParser)
            |= mulOpParser
            |= lazy (\_ -> termParser)
            |> backtrackable
        , succeed UnaryTerm
            |= lazy (\_ -> factorParser)
        ]


expressionParser : Parser Expression
expressionParser =
    Parser.oneOf
        [ succeed BinaryExpression
            |= lazy (\_ -> termParser)
            |= addOpParser
            |= lazy (\_ -> expressionParser)
            |> backtrackable
        , succeed UnaryExpression
            |= lazy (\_ -> termParser)
        ]


singleLineExpressionParser : Parser Expression
singleLineExpressionParser =
    expressionParser
        |. end
