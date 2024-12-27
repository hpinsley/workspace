module Evaluation.Engine exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (..)
import Models exposing (..)


evaluateExpression: Expression -> Dict String SymbolTableEntry -> Result String Float
evaluateExpression expression symbolTable =
    case expression of
        BinaryExpression factor addop expression2 -> 
            let
                result1 = evaluateFactor factor symbolTable
                result2 = evaluateExpression expression2 symbolTable
                final = Result.map2 (applyAddOp addop) result1 result2
            in
                final

        UnaryExpression factor ->
            evaluateFactor factor symbolTable

evaluateFactor: Factor -> Dict String SymbolTableEntry -> Result String Float
evaluateFactor factor symbolTable =
    case factor of
        IntFactor int -> 
            Ok (toFloat int)
        FloatFactor float ->
            Ok float
        SingleArgumentFunction function1 ->
            case function1 of
                Sin expr ->
                    evaluateExpression expr symbolTable |> Result.map sin
                Cos expr ->
                    evaluateExpression expr symbolTable |> Result.map cos
                Tan expr ->
                    evaluateExpression expr symbolTable |> Result.map tan

        VariableFactor variable ->
            case Dict.get variable symbolTable of
                Just entry ->
                    Ok entry.variableValue
                Nothing ->
                    Err ("Variable " ++ variable ++ " not found")

        BinaryFactor factor1 mulop factor2 ->
            let
                result1 = evaluateFactor factor1 symbolTable
                result2 = evaluateFactor factor2 symbolTable
                final = Result.map2 (applyMulOp mulop) result1 result2
            in
                final

applyMulOp: MulOp -> Float -> Float -> Float
applyMulOp mulOp x y =
    let
        _ = Debug.log ("Parsing " ++ String.fromFloat x ++ " and " ++ String.fromFloat y)
    in
        case mulOp of
            Times -> x * y
            Divide -> x / y

applyAddOp: AddOp -> Float -> Float -> Float
applyAddOp addOp x y =
    let
        _ = Debug.log ("Parsing " ++ String.fromFloat x ++ " and " ++ String.fromFloat y)
    in
        case addOp of
            Plus -> x + y
            Minus -> x - y
