module Utils exposing (..)

import Dict
import Evaluation.Engine exposing (..)
import Matrix exposing (..)
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
import Time exposing (..)


dotProductV2 : Vector2D -> Vector2D -> Float
dotProductV2 (Vec2D x1 y1) (Vec2D x2 y2) =
    x1 * x2 + y1 * y2

magnitudeV2 : Vector2D -> Float
magnitudeV2 (Vec2D x y) =
    sqrt(x*x + y*y)

-- Get the angle FROM v1 to v2.  We assume standard math coordinates

getAngleBetweenTwo2DVectors : Vector2D -> Vector2D -> Float
getAngleBetweenTwo2DVectors v1 v2 =
    let
        dp = dotProductV2 v1 v2 |> Debug.log "dot-product"
        m1 = magnitudeV2 v1 |> Debug.log "magnitude of v1"
        m2 = magnitudeV2 v2 |> Debug.log "magnitude of v2"
        magProduct = m1 * m2
        cosValue = if magProduct == 0 then 1.0 else (dp / magProduct)
        radianResult = acos (cosValue)
    in
        radianResult 

unitVectorI2D : Vector2D
unitVectorI2D = Vec2D 1 0

unitVectorJ2D : Vector2D
unitVectorJ2D = Vec2D 0 1

findPanelEntry : Model -> String -> Maybe PanelEntry
findPanelEntry model expression =
    case List.filter (\pe -> pe.expression == expression) model.panelEntries of
        [ pe ] ->
            Just pe

        _ ->
            Nothing

findActivePanelEntry : Model -> Maybe PanelEntry
findActivePanelEntry model =
    case model.activePlotEntry of
        Just activeExpression -> findPanelEntry model activeExpression
        Nothing -> Nothing

getVaryingVariables : PanelEntry -> List SymbolTableEntry
getVaryingVariables panelEntry =
    panelEntry.variables
        |> Dict.values
        |> List.filter (\pe -> pe.mayVary)


getVaryingVariableCount : PanelEntry -> Int
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


roundFloat : Int -> Float -> Float
roundFloat n f =
    let
        factor =
            (10 ^ n) |> toFloat

        temp =
            f * factor |> round |> toFloat

        result =
            temp / factor
    in
    result


x3dRotation : Float -> FloatMatrix
x3dRotation theta =
    case
        Matrix.fromLists
            [ [ 1.0, 0.0, 0.0 ]
            , [ 0.0, cos theta, negate (sin theta) ]
            , [ 0.0, sin theta, negate (cos theta) ]
            ]
    of
        Just m ->
            m

        Nothing ->
            Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."


y3dRotation : Float -> FloatMatrix
y3dRotation theta =
    case
        Matrix.fromLists
            [ [ cos theta, 0.0, sin theta ]
            , [ 0.0, 1.0, 0.0 ]
            , [ negate (sin theta), 0.0, cos theta ]
            ]
    of
        Just m ->
            m

        Nothing ->
            Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."


z3dRotation : Float -> FloatMatrix
z3dRotation theta =
    case
        Matrix.fromLists
            [ [ cos theta, negate (sin theta), 0.0 ]
            , [ sin theta, cos theta, 0.0 ]
            , [ 0.0, 0.0, 1 ]
            ]
    of
        Just m ->
            m

        Nothing ->
            Matrix.identity 3 |> Debug.log "Error creating matrix.  Returning identity matrix."


xyzRotation : Float -> Float -> Float -> FloatMatrix
xyzRotation xTheta yTheta zTheta =
    x3dRotation xTheta
        |> matrixMultiply (y3dRotation yTheta)
        |> matrixMultiply (z3dRotation zTheta)


printMatrix : String -> FloatMatrix -> ()
printMatrix message m =
    let
        _ =
            Matrix.pretty (\v -> Debug.toString v) m |> Debug.log (message ++ ": ")
    in
    ()


matrixMultiply : FloatMatrix -> FloatMatrix -> FloatMatrix
matrixMultiply leftMatrix rightMatrix =
    case Matrix.dot leftMatrix rightMatrix of
        Just m ->
            m

        Nothing ->
            Matrix.identity 3 |> Debug.log "Failed matrix multiply"


transposeVector : FloatMatrix -> GeneralVector -> GeneralVector
transposeVector m v =
    let
        vectorAsMatrix =
            vectorToMatrix v

        product =
            matrixMultiply m vectorAsMatrix
    in
    product |> matrixToVector

vector2DToGeneralVector: Vector2D -> GeneralVector
vector2DToGeneralVector (Vec2D x y) =
    [x,y]

vector3DToGeneralVector: Vector3D -> GeneralVector
vector3DToGeneralVector (Vec3D x y z) =
    [x,y,z]

generalVectorToVector2D: GeneralVector -> Vector2D
generalVectorToVector2D vector =
    case vector of 
        x :: y :: [] -> Vec2D x y
        _ -> Vec2D 0 0  |> Debug.log ("ERROR: generalVectorToVector2D invoked for a GeneralVector of length " ++ (vector |> List.length |> String.fromInt) ++ ".  Was expecting 2 items only")

generalVectorToVector3D: GeneralVector -> Vector3D
generalVectorToVector3D vector =
    case vector of 
        x :: y :: z :: [] -> Vec3D x y z
        _ -> Vec3D 0 0 0 |> Debug.log ("ERROR: generalVectorToVector3D invoked for a GeneralVector of length " ++ (vector |> List.length |> String.fromInt) ++ ".  Was expecting 3 items only")

generalLineSegmentToLineSegment3D : GeneralLineSegment -> ThreeDLineSegment
generalLineSegmentToLineSegment3D (vector1, vector2) =
    LineSeg3D (generalVectorToVector3D vector1) (generalVectorToVector3D vector2)

generalLineSegmentToLineSegment2D : GeneralLineSegment -> TwoDLineSegment
generalLineSegmentToLineSegment2D (vector1, vector2) =
    LineSeg2D (generalVectorToVector2D vector1) (generalVectorToVector2D vector2)

multiply3DData : FloatMatrix -> List Vector3D -> List Vector3D
multiply3DData m vectorList =
    vectorList
        |> List.map vector3DToGeneralVector 
        |> List.map (transposeVector m)
        |> List.map generalVectorToVector3D

multiply3DSingleVector : FloatMatrix -> Vector3D -> Vector3D
multiply3DSingleVector m vector =
    vector
        |> vector3DToGeneralVector 
        |> transposeVector m
        |> generalVectorToVector3D

multiply3DLineSegment : FloatMatrix -> ThreeDLineSegment -> ThreeDLineSegment
multiply3DLineSegment m (LineSeg3D from to) =
    LineSeg3D (multiply3DSingleVector m from) (multiply3DSingleVector m to)

multiply3DDataGeneralVectors : FloatMatrix -> List GeneralVector -> List GeneralVector
multiply3DDataGeneralVectors m vectorList =
    vectorList |> List.map (transposeVector m)

isPascalCased : String -> Bool
isPascalCased s =
    case String.uncons s of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


vectorToMatrix : GeneralVector -> FloatMatrix
vectorToMatrix values =
    case
        values
            |> Matrix.fromList 3 1
    of
        Just columnVector ->
            columnVector

        Nothing ->
            Matrix.identity 3 |> Debug.log "Failed to create column vector"


matrixToVector : FloatMatrix -> GeneralVector
matrixToVector m =
    m |> Matrix.toList


dropXComponent : List Float -> List Float
dropXComponent v =
    case List.tail v of
        Just t ->
            t

        Nothing ->
            [] |> Debug.log "Nothing to drop in dropXComponent"


dropYComponent : List Float -> List Float
dropYComponent v =
    case v of
        head :: tail ->
            head :: List.drop 1 tail

        [] ->
            [] |> Debug.log "Nothing to drop in dropYComponent"


dropZComponent : List Float -> List Float
dropZComponent v =
    case v of
        x :: y :: z ->
            [ x, y ]

        _ ->
            [] |> Debug.log "Nothing to drop in dropZComponent"

dropYFrom3DVector: Vector3D -> Vector2D
dropYFrom3DVector (Vec3D x y z) = Vec2D x z

dropYFrom3DLineSegment: ThreeDLineSegment -> TwoDLineSegment
dropYFrom3DLineSegment (LineSeg3D from to) = 
    let
        (Vec3D x1 y1 z1) = from
        (Vec3D x2 y2 z2) = to
        newFrom = Vec2D x1 z1
        newTo = Vec2D x2 z2
    in
        LineSeg2D newFrom newTo
        
isActivePlotPanel : Model -> PanelEntry -> Bool
isActivePlotPanel model panelEntry =
    case model.activePlotEntry of
        Nothing -> False
        Just expression -> panelEntry.expression == expression

applyFunctionToPanelEntryWithExpression: String -> (Model -> PanelEntry -> Model) -> Model -> Model
applyFunctionToPanelEntryWithExpression expression func model =
    case findPanelEntry model expression of
        Just pe ->
            func model pe
        Nothing ->
            let
                _ = Debug.log "ERROR: Cannot find panel with expression" expression
            in
                model

getRotationIncrement : Model -> Float
getRotationIncrement model =
    (rotationMaxValue - rotationMinValue) / model.rotations
