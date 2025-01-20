module Models exposing (..)

import Dict exposing (..)
import Matrix exposing (..)
import Parsing.ExpressionModels exposing (Expression, Variable)
import Time
import Html exposing (..)
import Color exposing (Color)

rotationMinValue = 0.0
rotationMaxValue = 2*pi
defaultRotationMs = 10.0
defaultRotations = 500.0

type alias ShadingRange =
    {
          minRed : Int
        , maxRed: Int
        , minGreen: Int
        , maxGreen: Int
        , minBlue: Int
        , maxBlue: Int
    }

-- General vector is of unspecified length
type alias GeneralVector =
    List Float

type Vector2D = Vec2D Float Float
type Vector3D = Vec3D Float Float Float

type alias GeneralLineSegment = (GeneralVector, GeneralVector)
type ThreeDLineSegment = LineSeg3D Vector3D Vector3D
type TwoDLineSegment = LineSeg2D Vector2D Vector2D
type TwoDColoredLineSegment = ColoredLineSeg2D TwoDLineSegment Color

type alias FloatMatrix =
    Matrix Float

type alias VariableLookup =
    Dict String Float


type alias SymbolTableDictionary =
    Dict String SymbolTableEntry

type alias MinMaxIncrement =
    {
          min: Float
        , max: Float
        , increment: Float
    }

type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | AddToPanel
    | DeleteExpression (String)
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
    | IncrementXAxisRotation PanelEntry 
    | IncrementYAxisRotation PanelEntry 
    | IncrementZAxisRotation PanelEntry 
    | DecrementXAxisRotation PanelEntry 
    | DecrementYAxisRotation PanelEntry 
    | DecrementZAxisRotation PanelEntry 
    | AutoRotateActivePanel
    | UpdatePanelEntryAutoRotate PanelEntry AutoRotate
    | IncreaseRotationSpeed
    | DecreaseRotationSpeed
    | SmootherRotations
    | JumpierRotations
    | SetXAxisRotationValue PanelEntry String
    | SetYAxisRotationValue PanelEntry String
    | SetZAxisRotationValue PanelEntry String
    | MouseDown String Int Int

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
    , mayVary : Bool
    }

type AutoRotate
    = NoAutoRotate
    | RotateX
    | RotateY
    | RotateZ

type alias Axis =
    { 
          axisName: String
        , rotationAngle : Float
    }


type alias PanelEntry =
    { expression : String
    , parsedExpression : Expression
    , variables : SymbolTableDictionary
    , isCollapsed : Bool
    , evaluation : Maybe Float
    , evaluatedPlotValues : List GeneralLineSegment
    , panelError : Maybe String
    , alignmentX : SvgAlignment
    , alignmentY : SvgAlignment
    , meetOrSlice : SvgAlignmentBehavor
    , xAxis : Axis
    , yAxis : Axis
    , zAxis : Axis
    , autoRotate: AutoRotate
    , currentPlot: Html Msg
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
    , activePlotEntry : Maybe String
    , rotationSpeed: Float
    , defaultRotationSpeed: Float
    , rotations: Float
    , shadingRange: ShadingRange
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
            }
    in
        ( inital_model, Cmd.none )
