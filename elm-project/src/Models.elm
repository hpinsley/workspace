module Models exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (Expression, Variable)
import Time


type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | AddToPanel
    | DeleteExpression String
    | EvaluateExpression String
    | UpdateVarValueBuffer PanelEntry SymbolTableEntry String
    | UpdateVarValue PanelEntry SymbolTableEntry String
    | TogglePanelEntry PanelEntry


type alias SymbolTableEntry =
    { variable : Variable
    , variableValue : Float
    , textInput : String
    , errMsg : Maybe String
    }


type alias PanelEntry =
    { expression : String
    , parsedExpression : Expression
    , variables : Dict String SymbolTableEntry
    , isCollapsed : Bool
    , evaluation : Maybe Float
    }


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    , parseErrors : String
    , variables : Dict String Variable
    , panelEntries : List PanelEntry
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing, expression = Nothing, parsedExpression = Nothing, parseErrors = "", variables = Dict.empty, panelEntries = [] }
    in
    ( inital_model, Cmd.none )
