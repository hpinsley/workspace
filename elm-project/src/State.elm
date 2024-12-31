module State exposing (..)

import Dict
import Evaluation.Engine exposing (..)
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Parsing.VariableExtraction exposing (extractVariablesFromExpression)
import Task
import Time exposing (..)
import Parser exposing (float)
import Debug exposing (toString)


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
                    updatePanelEntry expr evaluatePanel model
            in
            ( m, Cmd.none )

        TogglePanelEntry panelEntry ->
            let
                m =
                    updatePanelEntry panelEntry.expression (\pe -> { pe | isCollapsed = not pe.isCollapsed }) model
            in
            ( m, Cmd.none )

        UpdateVarStartValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | startValueBuffer = value }) model
            in
            ( m, Cmd.none )

        UpdateVarStartValue panelEntry symbolTableEntry _ ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryStartValue model
            in
            ( m, Cmd.none )


        UpdateVarEndValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | endValueBuffer = value }) model
            in
            ( m, Cmd.none )

        UpdateVarEndValue panelEntry symbolTableEntry _ ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryEndValue model
            in
            ( m, Cmd.none )

        UpdateVarIncrementValueBuffer panelEntry symbolTableEntry value ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | incrementValueBuffer = value }) model
            in
            ( m, Cmd.none )

        UpdateVarIncrementValue panelEntry symbolTableEntry _ ->
            let
                m =
                    updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryIncrementValue model
            in
            ( m, Cmd.none )

        Plot ->
            ( plotModel model, Cmd.none )

plotModel : Model -> Model
plotModel model =
    let
        panelEntries = model.panelEntries |> List.map plotPanelEntry
    in
        { model | panelEntries = panelEntries }

plotPanelEntry : PanelEntry -> PanelEntry
plotPanelEntry panelEntry =
    let
        values = panelEntry.variables |> Dict.values |> iterateVariables
                    |> Debug.log "Values: "
        
        varialbeNames = panelEntry.variables |> Dict.values |> List.map .variable
        named = values |> List.map (\vArray -> List.map2 (\n v -> (n, v)) varialbeNames vArray)
            |> Debug.log "Named: "
    in
        -- Debug.log debugString
        panelEntry

iterateVariables : List SymbolTableEntry -> List (List Float)
iterateVariables variables =
    let
        result = case variables of
            [] -> [[]]
            variable :: rest ->
                let
                    width = variable.endValue - variable.startValue |> Debug.log "Width:"
                    steps = width / (variable.incrementValue |> Debug.log "Increment: ") |> ceiling
                    
                    multipliers = List.range 0 steps |> List.map toFloat
                    values = List.map (\m -> variable.startValue + (m * variable.incrementValue)) multipliers
                    childValues = iterateVariables rest
                in
                    values |> List.map (\v -> List.map (\cv -> v :: cv) childValues) |> List.concat
    in
        result

evaluatePanel : PanelEntry -> PanelEntry
evaluatePanel panelEntry =
    let
        result =
            evaluateExpression panelEntry.parsedExpression panelEntry.variables
    in
    case result of
        Ok value ->
            { panelEntry | evaluation = Just value }

        Err _ ->
            panelEntry



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


updateSymbolTableEntryStartValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryStartValue entry =
    case String.toFloat entry.startValueBuffer of
        Just value ->
            { entry | startValue = value, currentValue = value, errMsg = Nothing }

        Nothing ->
            { entry | errMsg = Just "Invalid value." }

updateSymbolTableEntryEndValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryEndValue entry =
    case String.toFloat entry.endValueBuffer of
        Just value ->
            { entry | endValue = value, errMsg = Nothing }

        Nothing ->
            { entry | errMsg = Just "Invalid value." }

updateSymbolTableEntryIncrementValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryIncrementValue entry =
    case String.toFloat entry.incrementValueBuffer of
        Just value ->
            { entry | incrementValue = value, errMsg = Nothing }

        Nothing ->
            { entry | errMsg = Just "Invalid value." }

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
                                        }
                                )
                    , isCollapsed = False
                    , evaluation = Nothing
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
