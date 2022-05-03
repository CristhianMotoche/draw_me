module Main exposing (..)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Line as CSL
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
  { previousPoint : C.Point
  , currentPoint : C.Point
  , mode : PaintMode
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point

init : Model
init =
  { currentPoint = ( 0, 0 )
  , previousPoint = ( 0, 0 )
  , mode = Disabled
  }

controlPoint (x1 , y1) (x2 , y2) =
    ((x2 - x1) / 2, (y2 - y1) / 2)

-- View

renderables ((cx, cy) as currentPoint) ((px, py) as previousPoint) =
    let cp = controlPoint previousPoint currentPoint
    in [ C.path currentPoint <| [ C.lineTo  previousPoint ] ]

view : Model -> H.Html Msg
view { currentPoint, previousPoint, mode } =
  H.div
    [ HA.style "display" "flex" ]
    [ C.toHtml
        (500, 500)
        [ M.onDown (.offsetPos >> StartAt)
        , M.onUp (.offsetPos >> EndAt)
        , M.onMove (.offsetPos >> MoveAt)
        , HA.style "border" "1px solid black"
        ]
        [ C.shapes
             [ CSL.lineCap CSL.RoundCap
             , CSL.lineWidth 3
             , (CS.stroke (Color.rgb255 100 100 10))
             ] (renderables currentPoint previousPoint)
        ]
    ]

-- Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    StartAt point -> { model | mode = Enabled, currentPoint = point, previousPoint = point }
    EndAt point -> { model | mode = Disabled, currentPoint = point,previousPoint = point }
    MoveAt point ->
        if model.mode == Enabled
        then { model | currentPoint = point, previousPoint = model.currentPoint }
        else model

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
