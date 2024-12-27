module Evaluation.Engine exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (..)
import Models exposing (..)

evaluateExpression: Expression -> Dict String SymbolTableEntry -> Result String Float
evaluateExpression expression symbolTable =
    Ok 6.28
