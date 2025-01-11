module Models exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (Expression, Variable)
import Time
import Matrix exposing (..)

type alias Vector = List Float
type alias FloatMatrix = Matrix Float
type alias VariableLookup = Dict String Float
type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | AddToPanel
    | DeleteExpression String
    | EvaluateExpression String
    | UpdateVarStartValueBuffer PanelEntry SymbolTableEntry String
    | UpdateVarEndValueBuffer PanelEntry SymbolTableEntry String
    | UpdateVarIncrementValueBuffer PanelEntry SymbolTableEntry String
    | UpdateVarStartValue PanelEntry SymbolTableEntry String
    | UpdateVarEndValue PanelEntry SymbolTableEntry String
    | UpdateVarIncrementValue PanelEntry SymbolTableEntry String
    | ToggleVarMayVary PanelEntry SymbolTableEntry
    | TogglePanelEntry PanelEntry
    | Plot PanelEntry
    | SetXAlignment PanelEntry SvgAlignment
    | SetYAlignment PanelEntry SvgAlignment
    | SetAlignmentBehavior PanelEntry SvgAlignmentBehavor


type alias SymbolTableEntry =
    { variable : Variable
    , currentValue : Float
    , startValue : Float
    , startValueBuffer : String
    , endValue : Float
    , endValueBuffer : String
    , incrementValue : Float
    , incrementValueBuffer : String
    , errMsg : Maybe String
    , mayVary: Bool
    }


type alias PanelEntry =
    { expression : String
    , parsedExpression : Expression
    , variables : Dict String SymbolTableEntry
    , isCollapsed : Bool
    , evaluation : Maybe Float
    , plotValues : List (VariableLookup)
    , evaluatedPlotValues : List (VariableLookup, Result String Float )
    , panelError : Maybe String
    , alignmentX : SvgAlignment
    , alignmentY : SvgAlignment
    , meetOrSlice : SvgAlignmentBehavor
    }


type SvgAlignment
    = AlignMin
    | AlignMid
    | AlignMax


type SvgAlignmentBehavor
    = Meet
    | Slice


type alias Model =
    { currentTime : Maybe Time.Posix
    , expression : Maybe String
    , parsedExpression : Maybe Expression
    , parseErrors : String
    , variables : Dict String Variable
    , panelEntries : List PanelEntry
    , activePlotEntry : Maybe PanelEntry
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        inital_model =
            { currentTime = Nothing
            , expression = Nothing
            , parsedExpression = Nothing
            , parseErrors = ""
            , variables = Dict.empty
            , panelEntries = []
            , activePlotEntry = Nothing
            }
    in
    ( inital_model, Cmd.none )
