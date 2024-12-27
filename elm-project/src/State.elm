module State exposing (..)

import Dict
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
import Evaluation.Engine exposing (..)
import Parsing.ExpressionParsers as ExpressionParsers
import Parsing.VariableExtraction exposing (extractVariablesFromExpression)
import Task
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
                m = updatePanelEntry expr evaluatePanel model
            in
                ( m , Cmd.none )

        UpdateVarValueBuffer panelEntry symbolTableEntry value ->
            let
                m = updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable (\e -> { e | textInput = value}) model
            in
                ( m , Cmd.none )   

        UpdateVarValue panelEntry symbolTableEntry ->
            let
                m = updateSymbolTableEntry panelEntry.expression symbolTableEntry.variable updateSymbolTableEntryValue model
            in
                ( m , Cmd.none )
        
        TogglePanelEntry panelEntry ->
            let
                m = updatePanelEntry panelEntry.expression (\pe -> { pe | isCollapsed = not pe.isCollapsed }) model
            in
                ( m , Cmd.none )

evaluatePanel : PanelEntry -> PanelEntry
evaluatePanel panelEntry =
    let
        result = evaluateExpression panelEntry.parsedExpression panelEntry.variables
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
        mapper = \pe -> { 
                            pe | variables = Dict.map (\k -> \v ->
                            if k == variableToMatch
                                then mapFunc v
                                else v
                        ) pe.variables 
                        }
        m = updatePanelEntry expressionToMatch mapper model
    in
        m


updateSymbolTableEntryValue : SymbolTableEntry -> SymbolTableEntry
updateSymbolTableEntryValue entry =
    case String.toFloat entry.textInput of
        Just value ->
            { entry | variableValue = value, errMsg = Nothing, textInput = "" }

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
                    , variables = model.variables |> Dict.map (\_ -> \v -> { variable = v, variableValue = 0.0, errMsg = Nothing, textInput = "" })
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
