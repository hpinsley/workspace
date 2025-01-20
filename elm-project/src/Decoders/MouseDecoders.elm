module Decoders.MouseDecoders exposing (..)

import Models exposing (..)
import Json.Decode as Decode

mouseEventDecoder: Decode.Decoder MouseEvent
mouseEventDecoder =
    Decode.map3 MouseEvent 
                    (Decode.field "target" targetDecoder)
                    (Decode.field "offsetX" Decode.int)
                    (Decode.field "offsetY" Decode.int)

targetDecoder: Decode.Decoder EventTarget
targetDecoder =
    Decode.map2 EventTarget
        (Decode.field "id" Decode.string)
        (Decode.field "nodeName" Decode.string)

mouseMessageDecoder : (MouseEvent -> Msg) -> Decode.Decoder Msg
mouseMessageDecoder mouseEvent =
    mouseEventDecoder |> Decode.map mouseEvent

mouseDownDecoder : Decode.Decoder Msg
mouseDownDecoder =
    mouseMessageDecoder MouseDown

mouseUpDecoder : Decode.Decoder Msg
mouseUpDecoder =
    mouseMessageDecoder MouseUp
