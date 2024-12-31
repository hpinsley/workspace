module Evaluation.Engine exposing (..)

import Dict exposing (..)
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)


evaluateExpression : Expression -> (String -> Result String Float) -> Result String Float
evaluateExpression expression symbolLookup =
    case expression of
        BinaryExpression factor addop expression2 ->
            let
                result1 =
                    evaluateFactor factor symbolLookup

                result2 =
                    evaluateExpression expression2 symbolLookup

                final =
                    Result.map2 (applyAddOp addop) result1 result2
            in
            final

        UnaryExpression factor ->
            evaluateFactor factor symbolLookup


evaluateFactor : Factor -> (String -> Result String Float) -> Result String Float
evaluateFactor factor symbolLookup =
    case factor of
        IntFactor int ->
            Ok (toFloat int)

        FloatFactor float ->
            Ok float

        SingleArgumentFunction function1 ->
            case function1 of
                Sin expr ->
                    evaluateExpression expr symbolLookup |> Result.map sin

                Cos expr ->
                    evaluateExpression expr symbolLookup |> Result.map cos

                Tan expr ->
                    evaluateExpression expr symbolLookup |> Result.map tan

        VariableFactor variable ->
            symbolLookup variable

        BinaryFactor factor1 mulop factor2 ->
            let
                result1 =
                    evaluateFactor factor1 symbolLookup

                result2 =
                    evaluateFactor factor2 symbolLookup

                final =
                    Result.map2 (applyMulOp mulop) result1 result2
            in
            final


applyMulOp : MulOp -> Float -> Float -> Float
applyMulOp mulOp x y =
    let
        _ =
            Debug.log ("Parsing " ++ String.fromFloat x ++ " and " ++ String.fromFloat y)
    in
    case mulOp of
        Times ->
            x * y

        Divide ->
            x / y


applyAddOp : AddOp -> Float -> Float -> Float
applyAddOp addOp x y =
    let
        _ =
            Debug.log ("Parsing " ++ String.fromFloat x ++ " and " ++ String.fromFloat y)
    in
    case addOp of
        Plus ->
            x + y

        Minus ->
            x - y
