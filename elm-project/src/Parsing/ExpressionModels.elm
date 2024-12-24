module Parsing.ExpressionModels exposing (..)


type MulOp
    = Times
    | Divide


type AddOp
    = Plus
    | Minus


type alias Variable =
    String


type Function1
    = Sin Expression
    | Cos Expression
    | Tan Expression


type Factor
    = IntFactor Int
    | FloatFactor Float
    | SingleArgumentFunction Function1
    | VariableFactor Variable
    | BinaryFactor Factor MulOp Factor


type Expression
    = BinaryExpression Factor AddOp Expression
    | UnaryExpression Factor
