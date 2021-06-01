module Main exposing (..)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Tuple as T
import Html as H
import Html.Attributes as HA
import Html.Events.Extra.Mouse as M
import Debug
import Color


type PaintMode
  = Enabled
  | Disabled

type alias Model =
  { currentPoint : Point
  , mode : PaintMode
  }

type alias Point =
  { x: Float
  , y: Float
  }

type Msg
  = Paint Point
  | TogglePaintMode
  | NoOp

init : Model
init =
  { currentPoint = { x = 0, y = 0 }
  , mode = Disabled
  }

mkPoint : (Float, Float) -> Point
mkPoint (x, y) = { x = x, y = y }

-- View

view : Model -> H.Html Msg
view { currentPoint, mode } =
  H.div
    [ HA.style "display" "flex" ]
    [ C.toHtml
        (300, 300)
        [ M.onDown <|
            \_ -> TogglePaintMode,
          M.onUp <|
            \_ -> TogglePaintMode,
          M.onMove <|
            \ev ->
              if mode == Enabled
                 then Paint (mkPoint ev.offsetPos)
                 else NoOp,
          HA.style "border" "1px solid black"  ]
        [ C.shapes [ CS.fill (Color.rgb255 100 100 10) ] [ C.rect (currentPoint.x, currentPoint.y) 5 5 ] ]
    ]

-- Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    Paint point -> { model | currentPoint = point }
    TogglePaintMode -> { model | mode = toggle model.mode }
    NoOp -> model

toggle : PaintMode -> PaintMode
toggle m =
  case m of
    Enabled -> Disabled
    _ -> Enabled

-- Main

main : Program () Model Msg
main =
  Browser.sandbox
  { init = init
  , view = view
  , update = update
  }
