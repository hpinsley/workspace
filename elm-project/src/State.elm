module State exposing (..)

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
import Utils
import Parser exposing (variable)


defaultXAxisRotation = pi / 4.0
defaultYAxisRotation = 0.0
defaultZAxisRotation = pi / 4.0
defaultConstantValue = 1.0
defaultStartValue = -pi
defaultEndValue = pi
defaultIncrementValue = 0.08 -- Low values can cause stack overflow in Elm debugger if you have it enabled

defaultRotationMinValue = 0.0
defaultRotationMaxValue = 2*pi
defaultRotations = 32.0
defaultRotationIncrement = (defaultRotationMaxValue - defaultRotationMinValue) / defaultRotations


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        
        AutoRotateActivePanel ->
            let
                _ = Debug.log "Message" msg
            in         
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
                m2 = updatePlotModel panelEntry model
            in
                ( m2, Cmd.none )


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
            ( rotateXUp model panelEntry, Cmd.none)
                
        IncrementYAxisRotation panelEntry ->
            let
                newValue = min panelEntry.yAxis.minMaxIncrement.max (panelEntry.yAxis.rotationAngle + panelEntry.yAxis.minMaxIncrement.increment)
                axis = panelEntry.yAxis
                newAxis = ({ axis | rotationAngle = newValue })
                newPanelEntry = { panelEntry | yAxis = newAxis } |> plotPanelEntry

                m =
                    Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
                m2 = updatePlotModel newPanelEntry m
            in
                ( m2, Cmd.none )

        IncrementZAxisRotation panelEntry ->
            let
                newValue = min panelEntry.zAxis.minMaxIncrement.max (panelEntry.zAxis.rotationAngle + panelEntry.zAxis.minMaxIncrement.increment)
                axis = panelEntry.zAxis
                newAxis = ({ axis | rotationAngle = newValue })
                newPanelEntry = { panelEntry | zAxis = newAxis } |> plotPanelEntry
                m =
                    Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
                m2 = updatePlotModel newPanelEntry m
            in
                ( m2, Cmd.none )

        DecrementXAxisRotation panelEntry ->
            let
                newValue = max panelEntry.xAxis.minMaxIncrement.min (panelEntry.xAxis.rotationAngle - panelEntry.xAxis.minMaxIncrement.increment)
                axis = panelEntry.xAxis
                newAxis = ({ axis | rotationAngle = newValue })
                newPanelEntry = { panelEntry | xAxis = newAxis } |> plotPanelEntry

                m =
                    Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
                m2 = updatePlotModel newPanelEntry m
            in
                ( m2, Cmd.none ) |> Debug.log "Returned from IncrementXAxisRotation"

        DecrementYAxisRotation panelEntry ->
            let
                newValue = max panelEntry.yAxis.minMaxIncrement.min (panelEntry.yAxis.rotationAngle - panelEntry.yAxis.minMaxIncrement.increment)
                axis = panelEntry.yAxis
                newAxis = ({ axis | rotationAngle = newValue })
                newPanelEntry = { panelEntry | yAxis = newAxis } |> plotPanelEntry

                m =
                    Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
                m2 = updatePlotModel newPanelEntry m
            in
                ( m2, Cmd.none )

        DecrementZAxisRotation panelEntry ->
            let
                newValue = max panelEntry.zAxis.minMaxIncrement.min (panelEntry.zAxis.rotationAngle - panelEntry.zAxis.minMaxIncrement.increment)
                axis = panelEntry.zAxis
                newAxis = ({ axis | rotationAngle = newValue })
                newPanelEntry = { panelEntry | zAxis = newAxis } |> plotPanelEntry

                m =
                    Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
                m2 = updatePlotModel newPanelEntry m
            in
                ( m2, Cmd.none )

autoRotateActivePanel : Model -> Model
autoRotateActivePanel model =
    case model.activePlotEntry of
        Nothing -> model
        Just panelEntry -> 
            let
                updatedModel = case panelEntry.autoRotate of
                                    NoAutoRotate -> model
                                    RotateX -> rotateXUp model panelEntry
                                    _ -> model
            in
                updatedModel

rotateXUp: Model -> PanelEntry -> Model
rotateXUp model panelEntry =
    let
        newValue = min panelEntry.xAxis.minMaxIncrement.max (panelEntry.xAxis.rotationAngle + panelEntry.xAxis.minMaxIncrement.increment)
        axis = panelEntry.xAxis
        newAxis = ({ axis | rotationAngle = newValue })
        newPanelEntry = { panelEntry | xAxis = newAxis } |> plotPanelEntry
        m =
            Utils.updatePanelEntry panelEntry.expression (\_ -> newPanelEntry ) model
        m2 = updatePlotModel newPanelEntry m
    in
        m2

updatePlotModel: PanelEntry -> Model -> Model
updatePlotModel panelEntry model =
            let
                _ = Debug.log "Plotting" panelEntry.expression
                m =
                    Utils.updatePanelEntry panelEntry.expression plotPanelEntry model

                m2 =
                    case Utils.findPanelEntry m panelEntry.expression of
                        Just pe ->
                            if List.length pe.evaluatedPlotValues > 0 then
                                { m | activePlotEntry = Just pe }

                            else
                                m

                        Nothing ->
                            m
            in
                m2

plotPanelEntry : PanelEntry -> PanelEntry
plotPanelEntry panelEntry =
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
                    |> Debug.log "Constant lookup dict"

            fromToVaryingDicts = iterateSymbolTable panelEntry -- |> Debug.log "FromToVaryingDicts"

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
                            Err msg -> 0.0 |> Debug.log ("ERROR: " ++ msg)
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
            _ -> [] |> Debug.log ("ERROR: Unable to iterate " ++ String.fromInt varyingCount ++ "variables.")

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
        lookups |> Debug.log "Lookups"

iterateSymbolTableTwoVariables : SymbolTableEntry -> SymbolTableEntry -> List (VariableLookup, VariableLookup)
iterateSymbolTableTwoVariables v1 v2 =
    let
        v1Points = generateVariableRange v1 -- |> Debug.log "v1Points"
        v2Points = generateVariableRange v2 -- |> Debug.log "v2Points"
        v1ToPoints = v1Points |> List.drop 1
        v2ToPoints = v2Points |> List.drop 1
        v1LineSegs = List.map2 (\from to -> Vec2D from to) v1Points v1ToPoints -- |> Debug.log "v1LineSegs"
        v2LineSegs = List.map2 (\from to -> Vec2D from to) v2Points v2ToPoints -- |> Debug.log "v2LineSegs"

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
        lookups -- |> Debug.log "iterateSymbolTableTwoVariables Result"

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
                    , xAxis = { axisName = "X", rotationAngle = defaultXAxisRotation, minMaxIncrement = { min=defaultRotationMinValue, max=defaultRotationMaxValue, increment=defaultRotationIncrement } }
                    , yAxis = { axisName = "Y", rotationAngle = defaultYAxisRotation, minMaxIncrement = { min=defaultRotationMinValue, max=defaultRotationMaxValue, increment=defaultRotationIncrement} }
                    , zAxis = { axisName = "Z", rotationAngle = defaultZAxisRotation, minMaxIncrement = { min=defaultRotationMinValue, max=defaultRotationMaxValue, increment=defaultRotationIncrement }}
                    , autoRotate = NoAutoRotate }
            in
                { model | panelEntries = newPanelEntry :: model.panelEntries, expression = Nothing, parsedExpression = Nothing, variables = Dict.empty }


tickModel : Model -> Time.Posix -> Model
tickModel model theTime =
    { model | currentTime = Just theTime }


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        sub1 = every 5000.0 (\_ -> AutoRotateActivePanel)
    in
        sub1

-- 1every 1000.0 Tick


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
