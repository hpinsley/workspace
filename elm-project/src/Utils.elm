module Utils exposing (..)

import Debug exposing (toString)
import Dict
import Evaluation.Engine exposing (..)
import List exposing (reverse)
import List.Cartesian
import Models exposing (..)
import Parser exposing (float)
import Parsing.ExpressionModels exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Parsing.VariableExtraction exposing (extractVariablesFromExpression)
import Set exposing (Set)
import Time exposing (..)


findPanelEntry : Model -> String -> Maybe PanelEntry
findPanelEntry model expression =
    case List.filter (\pe -> pe.expression == expression) model.panelEntries of
        [ pe ] ->
            Just pe

        _ ->
            Nothing

getVaryingVariables: PanelEntry -> List SymbolTableEntry
getVaryingVariables panelEntry =
    panelEntry.variables
        |> Dict.values
        |> List.filter (\pe -> pe.mayVary)

getVaryingVariableCount: PanelEntry -> Int
getVaryingVariableCount panelEntry =
    panelEntry |> getVaryingVariables |> List.length


-- This function updates a specific PanelEntry in the model's panelEntries list.
-- It takes three arguments:
-- 1. expressionToMatch: A String representing the expression to match.
-- 2. mapFunc: A function that takes a PanelEntry and returns an updated PanelEntry.
-- 3. model: The current state of the model.
-- The function returns a new model with the updated panelEntries list.


updatePanelEntry : String -> (PanelEntry -> PanelEntry) -> Model -> Model
updatePanelEntry expressionToMatch mapFunc model =
    let
        panelEntries =
            model.panelEntries
                |> List.map
                    (\pe ->
                        if pe.expression == expressionToMatch then
                            mapFunc pe

                        else
                            pe
                    )
    in
    { model | panelEntries = panelEntries }


updateSymbolTableEntry : String -> Variable -> (SymbolTableEntry -> SymbolTableEntry) -> Model -> Model
updateSymbolTableEntry expressionToMatch variableToMatch mapFunc model =
    let
        mapper =
            \pe ->
                { pe
                    | variables =
                        Dict.map
                            (\k ->
                                \v ->
                                    if k == variableToMatch then
                                        mapFunc v

                                    else
                                        v
                            )
                            pe.variables
                }

        m =
            updatePanelEntry expressionToMatch mapper model
    in
        m
