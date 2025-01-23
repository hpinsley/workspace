module State exposing (..)

import Debug exposing (toString)
import Dict
import Evaluation.Engine exposing (..)
import List exposing (reverse)
import Models exposing (..)
import Parser exposing (float)
import Parsing.ExpressionModels exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Parsing.VariableExtraction exposing (extractVariablesFromExpression)
import Set exposing (Set)
import Time exposing (..)
import Utils
import Parser exposing (variable)
import Html exposing (..)
import Graphing.Plotter exposing (plot)
import Browser.Events
import Json.Decode as Decode
import Decoders.MouseDecoders as MouseDecoders
import MouseEventModels exposing (..)
import RuntimeEnvironmentModels exposing (..)

logEnabled = True

defaultIncrementValue = 0.10 -- Low values can cause stack overflow in Elm debugger if you have it enabled

-- defaultIncrementValue = 0.2 -- When you set webpack to include elm debugging

defaultXAxisRotation = pi / 4.0
defaultYAxisRotation = 0.0
defaultZAxisRotation = pi / 4.0
defaultConstantValue = 1.0
defaultStartValue = -pi
defaultEndValue = pi

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg  of
        MouseDown mouseEvent ->
            let
                _ = Debug.log "Mouse down" mouseEvent
            in
                ( {model | mouseDownEventInfo = Just mouseEvent }, Cmd.none)

        MouseUp mouseEvent ->
            (processMouseUpEvent model mouseEvent, Cmd.none)

        MouseMove mouseEvent ->
            let
                _ = Debug.log "Mouse move" mouseEvent
            in
                ( model, Cmd.none)

        AutoRotateActivePanel ->
            (autoRotateActivePanel model, Cmd.none)

        Tick currentTime ->
            ( tickModel model currentTime, Cmd.none )

        UpdateExpression expr ->
            let
                m =
                    { model | expression = Just expr }
            in
            parseModelExpression m

        AddToPanel ->
            let
                m =
                    addCurrentExpressionToPanel model
            in
            ( m, Cmd.none )

        DeleteExpression expr ->
            ( { model | panelEntries = List.filter (\pe -> pe.expression /= expr) model.panelEntries }, Cmd.none )

        EvaluateExpression expr ->
            let
                m =
                    Utils.updatePanelEntry expr evaluatePanel model
            in
            ( m, Cmd.none )

        TogglePanelEntry panelEntry ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | isCollapsed = not pe.isCollapsed }) model
            in
                ( m, Cmd.none )

        ToggleVarMayVary panelEntry symbolTableEntry ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | mayVary = not e.mayVary }) model
            in
            ( m, Cmd.none )

        UpdateVarStartValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | startValueBuffer = value }) model
            in
            ( m, Cmd.none )

        UpdateVarStartValue panelEntry symbolTableEntry _ ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryStartValue model
            in
            ( m, Cmd.none )

        UpdateVarEndValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | endValueBuffer = value }) model
            in
                ( m, Cmd.none )

        UpdateVarEndValue panelEntry symbolTableEntry _ ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryEndValue model
            in
            ( m, Cmd.none )

        UpdateVarIncrementValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | incrementValueBuffer = value }) model
            in
            ( m, Cmd.none )

        UpdateVarIncrementValue panelEntry symbolTableEntry _ ->
            let
                m =
                    Utils.updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryIncrementValue model
            in
            ( m, Cmd.none )

        Plot panelEntry ->
            let
                m1 = { model | activePlotEntry = Just panelEntry.expression }
                m2 = recomputeFunctionValuesForAPanelAndModel panelEntry m1
                m3 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m2
            in      
                ( m3, Cmd.none )

        SetXAlignment panelEntry alignment ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | alignmentX = alignment }) model
            in
            -- ( m, Cmd.none )
            update (Plot panelEntry) m

        SetYAlignment panelEntry alignment ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | alignmentY = alignment }) model
            in
            update (Plot panelEntry) m

        SetAlignmentBehavior panelEntry alignmentBehavior ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | meetOrSlice = alignmentBehavior }) model
            in
            update (Plot panelEntry) m

        IncrementXAxisRotation panelEntry ->
            let

                m = rotateXUp model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)
                
        IncrementYAxisRotation panelEntry ->
            let
                m = rotateYUp model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)

        IncrementZAxisRotation panelEntry ->
            let
                m = rotateZUp model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)

        DecrementXAxisRotation panelEntry ->
            let

                m = rotateXDown model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)
    
        DecrementYAxisRotation panelEntry ->
            let
                m = rotateYDown model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)

        DecrementZAxisRotation panelEntry ->
            let
                m = rotateZDown model panelEntry
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                (m2, Cmd.none)

        UpdatePanelEntryAutoRotate panelEntry autoRotateType ->
            let
                m = updatePanelEntryAutoRotate model panelEntry autoRotateType
                m2 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m
            in
                                
                (m2, Cmd.none)

        IncreaseRotationSpeed ->
            ({ model | rotationSpeed = 0.9 * model.rotationSpeed}, Cmd.none)

        DecreaseRotationSpeed ->
            ({ model | rotationSpeed = 1.1 * model.rotationSpeed}, Cmd.none)

        SmootherRotations ->
            ({ model | rotations = 1.1 * model.rotations}, Cmd.none)
        
        JumpierRotations ->
            ({ model | rotations = 0.9 * model.rotations}, Cmd.none)

        SetXAxisRotationValue panelEntry value ->
            let
                m = setAxisRotationValue model panelEntry panelEntry.xAxis value (\pe axis -> { pe | xAxis = axis })
            in
                ( m, Cmd.none)
        
        SetYAxisRotationValue panelEntry value ->
            let
                m = setAxisRotationValue model panelEntry panelEntry.yAxis value (\pe axis -> { pe | yAxis = axis })
            in
                ( m, Cmd.none)
        
        SetZAxisRotationValue panelEntry value ->
            let
                m = setAxisRotationValue model panelEntry panelEntry.zAxis value (\pe axis -> { pe | zAxis = axis })
            in
                ( m, Cmd.none)

processMouseUpEvent : Model -> MouseEvent -> Model
processMouseUpEvent model mouseUpEvent =
    let
        _ = Debug.log "Mouse up" mouseUpEvent
        m1 = {model | mouseDownEventInfo = Nothing }
        m2 = case model.mouseDownEventInfo of
                Nothing -> m1
                Just mouseDownEvent ->
                    computeMoveInfo m1 mouseDownEvent mouseUpEvent
    in
        m2

computeMoveInfo : Model -> MouseEvent -> MouseEvent -> Model
computeMoveInfo model mouseDown mouseUp =
    let
        _ = Debug.log "DOWN:" mouseDown
        _ = Debug.log "  UP:" mouseUp

        -- Y values on the screen increase downward, and I want to reverse that
        deltaVector = Vec2D 
                            (toFloat (mouseUp.screenX - mouseDown.screenX))
                            (toFloat (mouseUp.screenY - mouseDown.screenY) |> negate)
        _ = Debug.log "Delta" deltaVector
        i = Utils.unitVectorI2D
        radiansFromI = Utils.getAngleBetweenTwo2DVectors i deltaVector |> Debug.log "Radians"
        
        -- acos's range is from only from 0 to pi.  We can adjust for this ambiguity here
        (Vec2D _ deltaY) = deltaVector
        twopi = 2*pi
        adjustedRadians = (if deltaY >= 0.0 then radiansFromI else (-1.0 * radiansFromI + twopi)) |> Debug.log "Adjusted"

        deltaMagnitude = Utils.magnitudeV2 deltaVector

        -- Now we map the movement into 2pi/6 sectors.
        sectorSize = twopi / 6.0

        sectorStartStop = List.range 0 5
                            |> List.map (\secIndex -> (secIndex, (toFloat secIndex) * sectorSize, (toFloat(secIndex+1)) * sectorSize))
                            |> Debug.log "SectorStartStop"
        
        matchingSecIndex = sectorStartStop
                            |> List.filter (\(secIndex, start, stop) -> adjustedRadians >= start && adjustedRadians < stop)
                            |> List.head
                            |> Maybe.map (\(secIndex, _, _) -> secIndex)
                            |> Debug.log "matchingSectorInfo"

        sectorMovement: SectorMovement
        sectorMovement = case matchingSecIndex of
                            Nothing -> SectorIncrementX 0.0
                            Just sectorIndex ->
                                if      sectorIndex == 0 then SectorIncrementZ deltaMagnitude
                                else if sectorIndex == 1 then SectorIncrementX deltaMagnitude
                                else if sectorIndex == 2 then SectorIncrementY deltaMagnitude
                                else if sectorIndex == 3 then SectorIncrementZ -deltaMagnitude
                                else if sectorIndex == 4 then SectorIncrementX -deltaMagnitude
                                else SectorIncrementY -deltaMagnitude
        _ = Debug.log "Sector Movement" sectorMovement
    in
        model

updatePanelEntryAutoRotate : Model -> PanelEntry -> AutoRotate -> Model
updatePanelEntryAutoRotate model panelEntry autoRotateType =
    let
        _ = stateLog "updatePanelEntryAutoRotate called with" autoRotateType
        m2 = Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | autoRotate = autoRotateType }) model
    in
        m2

setAxisRotationValue : Model -> PanelEntry -> Axis -> String -> (PanelEntry -> Axis -> PanelEntry) -> Model
setAxisRotationValue model panelEntry axisToUpdate value setter =
            let
                m = case String.toFloat value of
                        Just v -> 
                            let
                                updatedAxis = { axisToUpdate | rotationAngle = v }
                                updatedPanelEntry = setter panelEntry updatedAxis
                                m2 = Utils.updatePanelEntry panelEntry.expression (\_ -> updatedPanelEntry) model
                                m3 = Utils.applyFunctionToPanelEntryWithExpression panelEntry.expression createUpdatedInstructions m2
                            in
                                m3
                                
                        Nothing ->
                            model
            in
                m

autoRotateActivePanel : Model -> Model
autoRotateActivePanel model =
    case Utils.findActivePanelEntry model of
        Nothing -> model
        Just panelEntry -> 
            let
                m = case panelEntry.autoRotate of
                                    NoAutoRotate -> model
                                    
                                    RotateX -> 
                                        let
                                            rotatedModel = rotateXUp model panelEntry
                                        in
                                            createUpdatedInstructions rotatedModel panelEntry

                                    RotateY -> 
                                        let
                                            rotatedModel = rotateYUp model panelEntry
                                        in
                                            createUpdatedInstructions rotatedModel panelEntry

                                    RotateZ -> 
                                        let
                                            rotatedModel = rotateZUp model panelEntry
                                        in
                                            createUpdatedInstructions rotatedModel panelEntry
            in
                m

createUpdatedInstructions: Model -> PanelEntry -> Model
createUpdatedInstructions model panelEntry =
    let
        _ = Debug.log "Recomputing SVG" ""
        newPlottingHtml = plot model panelEntry
        m2 = Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | currentPlot = newPlottingHtml }) model
    in
        m2

rotateXUp: Model -> PanelEntry -> Model
rotateXUp model panelEntry =
    rotateAxisUp model panelEntry (\pe -> pe.xAxis) (\newAxis pe -> { pe | xAxis = newAxis})

rotateYUp: Model -> PanelEntry -> Model
rotateYUp model panelEntry =
    rotateAxisUp model panelEntry (\pe -> pe.yAxis) (\newAxis pe -> { pe | yAxis = newAxis})

rotateZUp: Model -> PanelEntry -> Model
rotateZUp model panelEntry =
    rotateAxisUp model panelEntry (\pe -> pe.zAxis) (\newAxis pe -> { pe | zAxis = newAxis})

rotateXDown: Model -> PanelEntry -> Model
rotateXDown model panelEntry =
    rotateAxisDown model panelEntry (\pe -> pe.xAxis) (\newAxis pe -> { pe | xAxis = newAxis})

rotateYDown: Model -> PanelEntry -> Model
rotateYDown model panelEntry =
    rotateAxisDown model panelEntry (\pe -> pe.yAxis) (\newAxis pe -> { pe | yAxis = newAxis})

rotateZDown: Model -> PanelEntry -> Model
rotateZDown model panelEntry =
    rotateAxisDown model panelEntry (\pe -> pe.zAxis) (\newAxis pe -> { pe | zAxis = newAxis})

rotateAxisUp: Model -> PanelEntry -> (PanelEntry -> Axis) -> (Axis -> PanelEntry -> PanelEntry) -> Model
rotateAxisUp model panelEntry getter setter  =
    let
        increment = Utils.getRotationIncrement model
        axis = getter panelEntry
        addedValue = axis.rotationAngle + increment
        newValue = if addedValue > rotationMaxValue then rotationMinValue else addedValue
        newAxis = ({ axis | rotationAngle = newValue })
        newPanelEntry = setter newAxis panelEntry --{ panelEntry | xAxis = newAxis }
        m =
            Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
    in
        m

rotateAxisDown: Model -> PanelEntry -> (PanelEntry -> Axis) -> (Axis -> PanelEntry -> PanelEntry) -> Model
rotateAxisDown model panelEntry getter setter  =
    let
        increment = Utils.getRotationIncrement model
        axis = getter panelEntry
        addedValue = axis.rotationAngle - increment
        newValue = if addedValue < rotationMinValue then rotationMaxValue else addedValue
        newAxis = ({ axis | rotationAngle = newValue })
        newPanelEntry = setter newAxis panelEntry --{ panelEntry | xAxis = newAxis }
        m =
            Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
    in
        m

recomputeFunctionValuesForAPanelAndModel: PanelEntry -> Model -> Model
recomputeFunctionValuesForAPanelAndModel panelEntry model =
    let
        _ = stateLog "recomputeFunctionValuesForAPanelAndModel" ""
    in
        Utils.updatePanelEntry panelEntry.expression recomputeFunctionValuesForAPanel model

recomputeFunctionValuesForAPanel : PanelEntry -> PanelEntry
recomputeFunctionValuesForAPanel panelEntry =
    let
        _ = stateLog "recomputeFunctionValuesForAPanel" ""
    in
        if Utils.getVaryingVariableCount panelEntry > 2 then
            { panelEntry | panelError = Just "At most two VARYING variables can be plotted." }

        else
            let
                constantsLookup =
                    panelEntry.variables
                        |> Dict.values
                        |> List.filter (\v -> not v.mayVary)
                        |> List.map (\e -> ( e.variable, e.currentValue ))
                        |> Dict.fromList
                        -- |> stateLog "Constant lookup dict"

                fromToVaryingDicts = iterateSymbolTable panelEntry -- |> stateLog "FromToVaryingDicts"

                evaluated =
                    fromToVaryingDicts
                        |> List.map
                            (\(startDict, endDict) ->
                                (evaluateExpressionWithVariableDictionaries panelEntry.parsedExpression constantsLookup startDict,
                                evaluateExpressionWithVariableDictionaries panelEntry.parsedExpression constantsLookup endDict))
            in
                { panelEntry | evaluatedPlotValues = evaluated, panelError = Nothing }

evaluateExpressionWithVariableDictionaries : Expression -> VariableLookup -> VariableLookup -> GeneralVector
evaluateExpressionWithVariableDictionaries expression constantsLookup variableLookup =
    let
        computedResult = evaluateExpression expression
                                            (\variable ->
                                                case Dict.get variable variableLookup of
                                                    Just v ->
                                                        Ok v

                                                    Nothing ->
                                                        case Dict.get variable constantsLookup of
                                                            Just v ->
                                                                Ok v

                                                            Nothing ->
                                                                Err "Variable or constant not found."
                                            )

        functionValue = case computedResult of
                            Ok v -> v
                            Err msg -> 0.0 |> stateLog ("ERROR: " ++ msg)
        indepentVariableValues = Dict.values variableLookup
    in
        List.append indepentVariableValues [functionValue]

-- Create a list of Dictionary lookups for the VARYING variables


iterateSymbolTable : PanelEntry -> List (VariableLookup, VariableLookup)
iterateSymbolTable panelEntry =
    let 
        varying = panelEntry.variables |> Dict.values |> List.filter (\e -> e.mayVary)
        varyingCount = varying |> List.length
    in
        case varying of
            firstVariable :: secondVariable :: [] -> iterateSymbolTableTwoVariables firstVariable secondVariable
            singleVariable :: [] -> iterateSymbolTableSingleVariable singleVariable
            _ -> [] |> stateLog ("ERROR: Unable to iterate " ++ String.fromInt varyingCount ++ "variables.")

iterateSymbolTableSingleVariable : SymbolTableEntry -> List (VariableLookup, VariableLookup)
iterateSymbolTableSingleVariable variable =
    let
        width = variable.endValue - variable.startValue
        steps = width / variable.incrementValue |> ceiling
        stepRange = List.range 0 (steps - 1) |> List.map toFloat

        startStop = stepRange |> List.map (\step -> (step, step + 1))
        pairs = startStop |> List.map (\(from, to ) -> (variable.startValue + variable.incrementValue * from,  variable.startValue + variable.incrementValue * to))
        lookups = pairs |> List.map (\(p1, p2) -> (Dict.fromList [(variable.variable, p1)], Dict.fromList [(variable.variable, p2)]))
    in
        lookups -- |> stateLog "Lookups"

iterateSymbolTableTwoVariables : SymbolTableEntry -> SymbolTableEntry -> List (VariableLookup, VariableLookup)
iterateSymbolTableTwoVariables v1 v2 =
    let
        v1Points = generateVariableRange v1 -- |> stateLog "v1Points"
        v2Points = generateVariableRange v2 -- |> stateLog "v2Points"
        v1ToPoints = v1Points |> List.drop 1
        v2ToPoints = v2Points |> List.drop 1
        v1LineSegs = List.map2 (\from to -> Vec2D from to) v1Points v1ToPoints -- |> stateLog "v1LineSegs"
        v2LineSegs = List.map2 (\from to -> Vec2D from to) v2Points v2ToPoints -- |> stateLog "v2LineSegs"

        path1 = v1LineSegs
                    |> List.map (\(Vec2D x1 x2) -> 
                                    v2Points |> List.map (\y -> (Vec2D x1 y, Vec2D x2 y))
                                )
                    |> List.concat
        
        path2 = v2LineSegs
                    |> List.map (\(Vec2D y1 y2) -> 
                                    v1Points |> List.map (\x -> (Vec2D x y1, Vec2D x y2))
                                )
                    |> List.concat
        
        path = path1 ++ path2

        lookups = path
                    |> List.map (
                                    \((Vec2D x1 y1), (Vec2D x2 y2)) ->
                                        let
                                            fromLookup = Dict.fromList [(v1.variable, x1), (v2.variable, y1)]
                                            toLookup = Dict.fromList [ (v1.variable, x2), (v2.variable, y2)]
                                        in
                                            (fromLookup, toLookup)
                                )



        -- lookups = v1Points |> List.map (\(p1, p2) -> (Dict.fromList [(v1.variable, p1)], Dict.fromList [(v1.variable, p2)]))
    in
        -- List.append v1Values v2Values
        lookups -- |> stateLog "iterateSymbolTableTwoVariables Result"

generateVariableRange : SymbolTableEntry -> List (Float)
generateVariableRange v1 =
    let
        width = v1.endValue - v1.startValue
        steps = width / v1.incrementValue |> ceiling
        zeroToN = List.range 0 steps |> List.map toFloat
        v1PointRange = zeroToN |> List.map (\multiplier -> v1.startValue + v1.incrementValue * multiplier)
    in
        v1PointRange

evaluatePanel : PanelEntry -> PanelEntry
evaluatePanel panelEntry =
    let
        result =
            evaluateExpression panelEntry.parsedExpression
                (\variable ->
                    case Dict.get variable panelEntry.variables of
                        Just v ->
                            Ok v.currentValue

                        Nothing ->
                            Err "Variable not found."
                )
    in
    case result of
        Ok value ->
            { panelEntry | evaluation = Just value }

        Err _ ->
            panelEntry


parseAndEvaluateRangeExpression : String -> Result String Float
parseAndEvaluateRangeExpression expression =
    case ExpressionParsers.parseExpression expression of
        Ok parsedExpression ->
            evaluateExpression parsedExpression (\_ -> Err "Cannot use variables here")

        Err msg ->
            Err msg


updateSymbolTableEntryStartValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryStartValue entry =
    case parseAndEvaluateRangeExpression entry.startValueBuffer of
        Ok value ->
            { entry | startValue = value, currentValue = value, errMsg = Nothing }

        Err msg ->
            { entry | errMsg = Just msg }


updateSymbolTableEntryEndValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryEndValue entry =
    case parseAndEvaluateRangeExpression entry.endValueBuffer of
        Ok value ->
            { entry | endValue = value, errMsg = Nothing }

        Err msg ->
            { entry | errMsg = Just msg }


updateSymbolTableEntryIncrementValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryIncrementValue entry =
    case parseAndEvaluateRangeExpression entry.incrementValueBuffer of
        Ok value ->
            { entry | incrementValue = value, errMsg = Nothing }

        Err msg ->
            { entry | errMsg = Just msg }

addCurrentExpressionToPanel : Model -> Model
addCurrentExpressionToPanel model =
    case model.parsedExpression of
        Nothing ->
            model

        Just parsedExpression ->
            let
                newPanelEntry =
                    { expression =
                        case model.expression of
                            Nothing ->
                                ""

                            Just expr ->
                                expr
                    , parsedExpression = parsedExpression
                    , variables =
                        model.variables
                            |> Dict.map
                                (\_ ->
                                    \v ->
                                        let
                                            mayVary = not (Utils.isPascalCased v)
                                            startValue = if mayVary then defaultStartValue else defaultConstantValue
                                            endValue = defaultEndValue
                                            incValue = defaultIncrementValue

                                            entry = { variable = v
                                                    , errMsg = Nothing
                                                    , currentValue = startValue
                                                    , startValue = startValue
                                                    , startValueBuffer = if startValue == -pi then "-pi" else String.fromFloat startValue
                                                    , endValue = endValue
                                                    , endValueBuffer = if endValue == pi then "pi" else String.fromFloat endValue
                                                    , incrementValue = incValue
                                                    , incrementValueBuffer = String.fromFloat incValue
                                                    , mayVary = not (Utils.isPascalCased v)
                                                    }
                                        in
                                            entry
                                )
                    , isCollapsed = False
                    , evaluation = Nothing
                    , evaluatedPlotValues = []
                    , panelError = Nothing
                    , alignmentX = AlignMid
                    , alignmentY = AlignMid
                    , meetOrSlice = Meet
                    , xAxis = { axisName = "X", rotationAngle = defaultXAxisRotation }
                    , yAxis = { axisName = "Y", rotationAngle = defaultYAxisRotation }
                    , zAxis = { axisName = "Z", rotationAngle = defaultZAxisRotation }
                    , autoRotate = NoAutoRotate
                    , currentPlot = div [] [text "no plot"] }
            in
                { model | panelEntries = newPanelEntry :: model.panelEntries, expression = Nothing, parsedExpression = Nothing, variables = Dict.empty }


tickModel : Model -> Time.Posix -> Model
tickModel model theTime =
    { model | currentTime = Just theTime }


parseModelExpression : Model -> ( Model, Cmd Msg )
parseModelExpression model =
    case model.expression of
        Nothing ->
            ( { model | parseErrors = "Please enter an expression." }, Cmd.none )

        Just expression ->
            let
                parsedExpression =
                    ExpressionParsers.parseExpression expression
            in
            case parsedExpression of
                Ok goodExpression ->
                    ( { model
                        | parsedExpression = Just goodExpression
                        , variables = extractVariablesFromExpression goodExpression
                        , parseErrors = ""
                      }
                    , Cmd.none
                    )

                Err errmsg ->
                    ( { model | parseErrors = errmsg, variables = Dict.empty, parsedExpression = Nothing }, Cmd.none )

stateLog : String -> a -> a
stateLog msg obj =
    if logEnabled then (Debug.log msg obj) else obj

init : Decode.Value -> ( Model, Cmd Msg )
init runtimeFlags =
    let
        _ = Debug.log "flags" runtimeFlags
        runtimeEnv = case Decode.decodeValue MouseDecoders.runtimeEnvironmentDecoder runtimeFlags of
                Ok runtimeEnvironment ->
                    runtimeEnvironment
                Err msg ->
                    let
                        _ = Debug.log "Unable to decode runtime environment" msg
                    in
                        { screenX = 1, screenY = 1 }

        _ = Debug.log "Decoded" runtimeEnv

        inital_model =
            { currentTime = Nothing
            , screenX = runtimeEnv.screenX
            , screenY = runtimeEnv.screenY
            , expression = Nothing
            , parsedExpression = Nothing
            , parseErrors = ""
            , variables = Dict.empty
            , panelEntries = []
            , activePlotEntry = Nothing
            , rotationSpeed = defaultRotationMs
            , defaultRotationSpeed = defaultRotationMs
            , rotations = defaultRotations
            , shadingRange = {
                                  minRed = 0 
                                , maxRed = 240
                                , minGreen = 0
                                , maxGreen = 240
                                , minBlue = 0
                                , maxBlue = 240
                            }
            , mouseDownEventInfo = Nothing
            }
    in
        ( inital_model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- Only subscribe if the mouse button is down as if a drag while we are plotting and not auto-rotating
        mouseMoveSub = case Utils.findActivePanelEntry model of 
                        Nothing -> 
                            Sub.none
                        Just pe -> 
                            case pe.autoRotate of
                                NoAutoRotate ->
                                    case model.mouseDownEventInfo of
                                        Just eventInfo ->
                                            Browser.Events.onMouseMove MouseDecoders.mouseMoveDecoder
                                        Nothing ->
                                            Sub.none
                                _-> Sub.none


        -- TODO: If there is no active panel, skip the clock tick
        subs = [
                every model.rotationSpeed (\_ -> AutoRotateActivePanel)
                , Browser.Events.onMouseDown MouseDecoders.mouseDownDecoder
                , Browser.Events.onMouseUp MouseDecoders.mouseUpDecoder
                -- , mouseMoveSub
                ]
    in
        Sub.batch subs

    -- Sub.none
