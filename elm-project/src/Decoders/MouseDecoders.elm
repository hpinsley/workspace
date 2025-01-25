module Decoders.MouseDecoders exposing (..)

import Json.Decode as Decode
import Models exposing (..)
import MouseEventModels exposing (..)
import RuntimeEnvironmentModels exposing (..)


runtimeEnvironmentDecoder : Decode.Decoder RuntimeEnvironment
runtimeEnvironmentDecoder =
    Decode.map2 RuntimeEnvironment
        (Decode.field "screenX" Decode.int)
        (Decode.field "screenY" Decode.int)


mouseEventDecoder : Decode.Decoder MouseEvent
mouseEventDecoder =
    Decode.map7 MouseEvent
        (Decode.field "target" targetDecoder)
        (Decode.field "screenX" Decode.int)
        (Decode.field "screenY" Decode.int)
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


targetDecoder : Decode.Decoder EventTarget
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


mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    mouseMessageDecoder MouseMove


decodeRuntimeFlags : Decode.Value -> RuntimeEnvironmentModels.RuntimeEnvironment
decodeRuntimeFlags runtimeFlags =
    case Decode.decodeValue runtimeEnvironmentDecoder runtimeFlags of
        Ok runtimeEnvironment ->
            runtimeEnvironment

        Err msg ->
            let
                _ =
                    Debug.log "Unable to decode runtime environment" msg
            in
            { screenX = 1, screenY = 1 }
