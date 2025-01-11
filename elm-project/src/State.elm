module State exposing (..)

import Debug exposing (toString)
import Dict
import Evaluation.Engine exposing (..)
import List exposing (reverse)
import List.Cartesian
import Models exposing (..)
import Utils
import Parser exposing (float)
import Parsing.ExpressionModels exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Parsing.VariableExtraction exposing (extractVariablesFromExpression)
import Set exposing (Set)
import Time exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                ( m2, Cmd.none )

        SetXAlignment panelEntry alignment ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | alignmentX = alignment }) model
            in
                -- ( m, Cmd.none )
                (update (Plot panelEntry) m)

        SetYAlignment panelEntry alignment ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | alignmentY = alignment }) model
            in
                (update (Plot panelEntry) m)

        SetAlignmentBehavior panelEntry alignmentBehavior ->
            let
                m =
                    Utils.updatePanelEntry panelEntry.expression (\pe -> { pe | meetOrSlice = alignmentBehavior }) model
            in
                (update (Plot panelEntry) m)


plotPanelEntry : PanelEntry -> PanelEntry
plotPanelEntry panelEntry =
    if (Utils.getVaryingVariableCount panelEntry) > 2 then
        { panelEntry | panelError = Just "At most two VARYING variables can be plotted." }

    else
        let
            nonVaryingLookup =
                panelEntry.variables
                    |> Dict.values
                    |> List.filter (\v -> not v.mayVary)
                    |> List.map (\e -> (e.variable, e.currentValue))
                    |> Dict.fromList
                    |> Debug.log "Constant lookup dict"

            named =
                iterateSymbolTable panelEntry

            evaluated =
                    named |>
                            List.map
                                (\dict ->
                                    ( dict
                                    , evaluateExpression panelEntry.parsedExpression
                                        (\variable ->
                                            case Dict.get variable dict of
                                                Just v ->
                                                    Ok v

                                                Nothing ->
                                                    case Dict.get variable nonVaryingLookup of
                                                        Just v ->
                                                            Ok v
                                                        Nothing ->
                                                            Err "Variable or constant not found."
                                        )
                                    )
                                )
                            |> List.map (\(varlookup, floatResult) -> 
                                            case floatResult of
                                                Ok v -> (varlookup, v)
                                                Err msg -> (varlookup, 0.0) |> Debug.log ("ERROR: Evaluation failure: " ++ msg)
                                        )
                            |> List.map (\(varlookup, f) -> List.append (Dict.values varlookup) [f])
        in
        { panelEntry | plotValues = named, evaluatedPlotValues = evaluated, panelError = Nothing }


-- Create a list of Dictionary lookups for the VARYING variables
iterateSymbolTable : PanelEntry -> List VariableLookup
iterateSymbolTable panelEntry =
    let
        vars = Dict.values panelEntry.variables

        varyingVars =
            vars |> List.filter (\e -> e.mayVary)

        varyingVarNames =
            varyingVars |> List.filter (\e -> e.mayVary) |> List.map .variable

        values1 =
            iterateVariables [ [] ] varyingVars
                |> List.map reverse |> Debug.log "Value1"

        values2 = iterateVariables [[]] (List.reverse varyingVars) |> Debug.log "Value2"
        named1 =
            values1 |> List.map (\vArray -> List.map2 (\n v -> ( n, v )) varyingVarNames vArray |> Dict.fromList) |> Debug.log "named1"
        named2 =
            values2 |> List.map (\vArray -> List.map2 (\n v -> ( n, v )) varyingVarNames vArray |> Dict.fromList) |> Debug.log "named2"
        named3 = named1 ++ named2 |> Debug.log "named3"
        x1 =
            values1 |> List.map (\vArray -> List.map2 (\n v -> ( n, v )) varyingVarNames vArray) |> Debug.log "x1"
        x2 =
            values2 |> List.map (\vArray -> List.map2 (\n v -> ( n, v )) varyingVarNames vArray) |> Debug.log "x2"
        x3 = x1 ++ x2 |> Debug.log "x3"
    in
        named1 |> Debug.log "final"

-- TODO: I think this recursive method is the one that can blow the stack
iterateVariables : List Vector -> List SymbolTableEntry -> List Vector
iterateVariables sofar variables =
    case variables of
        [] ->
            sofar

        variable :: rest ->
            let
                width =
                    variable.endValue - variable.startValue

                steps =
                    width / variable.incrementValue |> ceiling

                multipliers =
                    List.range 0 steps |> List.map toFloat

                values =
                    if variable.mayVary
                    then
                        List.map (\m -> variable.startValue + (m * variable.incrementValue)) multipliers
                    else
                        [variable.currentValue]

                permuated =
                    case sofar of
                        [] ->
                            [ [] ]

                        _ ->
                            List.Cartesian.map2 (::) values sofar
            in
                iterateVariables permuated rest


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
                                        { variable = v
                                        , errMsg = Nothing
                                        , currentValue = 0.0
                                        , startValue = 0.0
                                        , startValueBuffer = ""
                                        , endValue = 0.0
                                        , endValueBuffer = ""
                                        , incrementValue = 0.0
                                        , incrementValueBuffer = ""
                                        , mayVary = not (Utils.isPascalCased v)
                                        }
                                )
                    , isCollapsed = False
                    , evaluation = Nothing
                    , plotValues = []
                    , evaluatedPlotValues = []
                    , panelError = Nothing
                    , alignmentX = AlignMid
                    , alignmentY = AlignMid
                    , meetOrSlice = Meet
                    }
            in
            { model | panelEntries = newPanelEntry :: model.panelEntries, expression = Nothing, parsedExpression = Nothing, variables = Dict.empty }


tickModel : Model -> Time.Posix -> Model
tickModel model theTime =
    { model | currentTime = Just theTime }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
