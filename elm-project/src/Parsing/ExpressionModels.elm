module Parsing.ExpressionModels exposing (..)

type MulOp
    = Times
    | Divide

type AddOp
    = Plus
    | Minus

type alias Argument = String

type Function1
    = Sin Argument
    | Cos Argument
    | Tan Argument

type Factor
    = IntFactor Int
    | FloatFactor Float
    | SingleArgumentFunction Function1
    | BinaryFactor Factor MulOp Factor

type Expression
    = BinaryExpression Factor AddOp Expression
    | UnaryExpression Factor
