module Parsing.VariableExtraction exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (..)


extractVariablesFromExpression : Expression -> Dict String Variable
extractVariablesFromExpression expression =
    extractVariableListFromExpression expression
        |> List.map (\v -> ( v, v ))
        |> Dict.fromList


extractVariableListFromFunction1 : Function1 -> List Variable
extractVariableListFromFunction1 f =
    case f of
        Sin expr ->
            extractVariableListFromExpression expr

        Cos expr ->
            extractVariableListFromExpression expr

        Tan expr ->
            extractVariableListFromExpression expr


extractVariableListFromFactor : Factor -> List Variable
extractVariableListFromFactor factor =
    case factor of
        IntFactor n ->
            []

        FloatFactor n ->
            []

        SingleArgumentFunction f ->
            extractVariableListFromFunction1 f

        VariableFactor v ->
            [ v ]

        BinaryFactor f1 mulOp f2 ->
            extractVariableListFromFactor f1 ++ extractVariableListFromFactor f2


extractVariableListFromExpression : Expression -> List Variable
extractVariableListFromExpression expression =
    case expression of
        BinaryExpression factor addOp rightExpression ->
            extractVariableListFromFactor factor ++ extractVariableListFromExpression rightExpression

        UnaryExpression factor ->
            extractVariableListFromFactor factor
