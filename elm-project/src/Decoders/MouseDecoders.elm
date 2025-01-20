module Decoders.MouseDecoders exposing (..)

import Models exposing (..)
import Json.Decode as Decode
import Task exposing (succeed)

type alias MouseEvent = 
    {
            target: Target
          , offsetX: Int
          , offsetY: Int
    }

type alias Target =
    {
        id: String
    }


mouseEventDecoder: Decode.Decoder MouseEvent
mouseEventDecoder =
    Decode.map3 MouseEvent 
                    (Decode.field "target" targetDecoder)
                    (Decode.field "offsetX" Decode.int)
                    (Decode.field "offsetY" Decode.int)

targetDecoder: Decode.Decoder Target
targetDecoder =
    Decode.field "id" Decode.string
    |> Decode.map Target

mouseDownDecoder : Decode.Decoder Msg
mouseDownDecoder =
    mouseEventDecoder |> Decode.map (\evt -> MouseDown (evt.target.id) evt.offsetX evt.offsetY)
    -- Decode.succeed (MouseDown "dummy")

