module MouseEventModels exposing (..)

type alias EventTarget =
    {
          id: String
        , nodeName: String
    }

type alias MouseEvent = 
    {
            target: EventTarget
          , screenX: Int
          , screenY: Int
          , clientX: Int
          , clientY: Int
          , offsetX: Int
          , offsetY: Int
    }
