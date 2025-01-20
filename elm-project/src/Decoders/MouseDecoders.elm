module Decoders.MouseDecoders exposing (..)

import Models exposing (..)
import Json.Decode as Decode
import Task exposing (succeed)

type alias MouseEvent = 
    {
        target: Target
    }

type alias Target =
    {
        id: String
    }


mouseEventDecoder: Decode.Decoder MouseEvent
mouseEventDecoder =
    Decode.field "target" targetDecoder
    |> Decode.map MouseEvent

targetDecoder: Decode.Decoder Target
targetDecoder =
    Decode.field "id" Decode.string
    |> Decode.map Target

mouseDownDecoder : Decode.Decoder Msg
mouseDownDecoder =
    mouseEventDecoder |> Decode.map (\evt -> MouseDown (evt.target.id))
    -- Decode.succeed (MouseDown "dummy")

