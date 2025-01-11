module Utils exposing (..)

import Dict
import Evaluation.Engine exposing (..)
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
import Time exposing (..)
import Matrix exposing (Matrix)


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

roundFloat: Int -> Float -> Float
roundFloat n f =
    let
        factor = (10^n) |> toFloat
        temp = f * factor |> round |> toFloat
        result = temp / factor
    in
        result

x3dRotation: Float -> (Matrix Float)
x3dRotation theta =
    case Matrix.fromLists [
                 [1.0, 0.0, 0.0]
                ,[0.0, cos theta, negate (sin theta)]
                ,[0.0, sin theta, negate (cos theta)] 
            ] of
                
        Just m -> m
        Nothing -> Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."

y3dRotation: Float -> (Matrix Float)
y3dRotation theta =
    case Matrix.fromLists [
                 [cos theta, 0.0, sin theta]
                ,[0.0, 1.0, 0.0]
                ,[negate (sin theta), 0.0, cos theta] 
            ] of
                
        Just m -> m
        Nothing -> Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."

z3dRotation: Float -> (Matrix Float)
z3dRotation theta =
    case Matrix.fromLists [
                 [cos theta, negate (sin theta), 0.0]
                ,[sin theta, cos theta, 0.0]
                ,[0.0, 0.0, 1] 
            ] of
                
        Just m -> m
        Nothing -> Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."

printMatrix: String -> (Matrix Float) -> ()
printMatrix message m =
    let
        _ = Matrix.pretty (\v -> Debug.toString v) m |> Debug.log (message ++ ": ")        
    in
        ()

multiply3DData: (Matrix Float) -> List(List Float) -> List(List Float)
multiply3DData m input =
    let
        multiplied = input |> List.map (\values ->
                            let
                                vector = make3DVector values

                                _ = vector |> printMatrix "m2"
                            in
                                values)
    in
        input
    
make3DVector: (List Float) -> (Matrix Float)
make3DVector values =
    case values 
        |> Matrix.fromList 3 1 of
                Just columnVector -> columnVector
                Nothing -> Matrix.identity 3 |> Debug.log "Failed to create column vector"


dropXComponent: List Float -> List Float
dropXComponent v =
    case List.tail v of
        Just t -> t
        Nothing -> [] |> Debug.log("Nothing to drop in dropXComponent")

dropYComponent: List Float -> List Float
dropYComponent v =
    case v of
        head :: tail -> head :: (List.drop 1 tail)
        [] -> [] |> Debug.log("Nothing to drop in dropYComponent")

dropZComponent: List Float -> List Float
dropZComponent v =
    case v of
        x ::  y :: z -> [x,y]
        _ -> [] |> Debug.log("Nothing to drop in dropZComponent")
