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
        
        Abs expr ->
            extractVariableListFromExpression expr

        Ln expr ->
            extractVariableListFromExpression expr



extractVariableListFromFactor : Factor -> List Variable
extractVariableListFromFactor factor =
    case factor of
        IntFactor _ ->
            []

        FloatFactor _ ->
            []

        SingleArgumentFunction f ->
            extractVariableListFromFunction1 f

        VariableFactor v ->
            [ v ]

        BinaryFactor f1 _ f2 ->
            extractVariableListFromFactor f1 ++ extractVariableListFromFactor f2

        Power f1 f2 ->
            extractVariableListFromFactor f1 ++ extractVariableListFromFactor f2

        ExpressionFactor expr ->
            extractVariableListFromExpression expr

        NegatedFactor f ->
            extractVariableListFromFactor f


extractVariableListFromTerm : Term -> List Variable
extractVariableListFromTerm term =
    case term of
        BinaryTerm leftFactor _ rightTerm ->
            extractVariableListFromFactor leftFactor ++ extractVariableListFromTerm rightTerm

        UnaryTerm factor ->
            extractVariableListFromFactor factor


extractVariableListFromExpression : Expression -> List Variable
extractVariableListFromExpression expression =
    case expression of
        BinaryExpression leftTerm _ rightExpression ->
            extractVariableListFromTerm leftTerm ++ extractVariableListFromExpression rightExpression

        UnaryExpression term ->
            extractVariableListFromTerm term
