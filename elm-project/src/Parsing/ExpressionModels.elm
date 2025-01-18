module Parsing.ExpressionModels exposing (..)

-- We want to move to this:
-- Expression   ::= Term (ADDOP Term)*
-- Term         ::= Factor (MULOP Factor)*
-- Factor       ::= NUMBER | VARIABLE | '(' Expression ')' | Factor '^' Factor
-- ADDOP        ::= '+' | '-'
-- MULOP        ::= '*' | '/'
-- NUMBER       ::= [0-9]+
-- VARIABLE     ::= [a-zA-Z]


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
    | Abs Expression


type Factor
    = IntFactor Int
    | FloatFactor Float
    | SingleArgumentFunction Function1
    | VariableFactor Variable
    | Power Factor Factor
    | BinaryFactor Factor MulOp Factor
    | ExpressionFactor Expression
    | NegatedFactor Factor


type Term
    = BinaryTerm Factor MulOp Term
    | UnaryTerm Factor


type Expression
    = BinaryExpression Term AddOp Expression
    | UnaryExpression Term
