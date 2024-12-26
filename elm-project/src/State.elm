module State exposing (..)

import Dict
import Models exposing (..)
import Parsing.ExpressionModels exposing (..)
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

        UpdateVarValueBuffer panelEntry symbolTableEntry value ->
            let
                panelEntries =
                    model.panelEntries
                        |> List.map
                            (\pe ->
                                if pe.expression == panelEntry.expression then
                                    { pe | variables = Dict.map ( 
                                        \k -> \v ->
                                            if k == symbolTableEntry.variable
                                                then { v | textInput = value }
                                                else v
                                        ) pe.variables }
                                else
                                    pe
                            )
            in
                ( { model | panelEntries = panelEntries }, Cmd.none )   

        UpdateVarValue panelEntry symbolTableEntry ->
            let
                panelEntries =
                    model.panelEntries
                        |> List.map
                            (\pe ->
                                if pe.expression == panelEntry.expression then
                                    { pe | variables = Dict.map ( 
                                        \k -> \v ->
                                            if k == symbolTableEntry.variable
                                                then updateSymbolTableEntryValue v
                                                else v
                                        ) pe.variables }
                                else
                                    pe
                            )
            in
                ( { model | panelEntries = panelEntries }, Cmd.none )   


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
