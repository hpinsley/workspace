module Parsing.ExpressionModels exposing (..)

type MulOp
    = Times
    | Divide

type AddOp
    = Plus
    | Minus

type Factor
    = IntFactor Int
    | FloatFactor Float
    | BinaryFactor Factor MulOp Factor

type Expression
    = BinaryExpression Factor AddOp Expression
    | UnaryExpression Factor
