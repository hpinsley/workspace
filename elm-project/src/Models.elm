module Models exposing (..)

import Dict exposing (..)
import Parsing.ExpressionModels exposing (Expression, Variable)
import Time


type Msg
    = Tick Time.Posix
    | UpdateExpression String
    | AddToPanel

type alias PanelEntry =
    { expression: String
    , parsedExpression : Expression
    , variables: Dict String Variable
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
