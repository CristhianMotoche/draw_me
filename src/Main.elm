module Main exposing (..)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Line as CSL
import Tuple as T
import Html as H
import Html.Attributes as HA
import Html.Events.Extra.Mouse as M
import Html.Events as HE
import Debug
import Color


type PaintMode
  = Enabled
  | Disabled
  | Cleared
  | Blocked

type alias Model =
  { previousPoint : C.Point
  , currentPoint : C.Point
  , mode : PaintMode
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point
  | Clear
  | Block

init : Model
init =
  { currentPoint = ( 0, 0 )
  , previousPoint = ( 0, 0 )
  , mode = Disabled
  }

-- View

renderables ((cx, cy) as currentPoint) ((px, py) as previousPoint) =
    [ C.path currentPoint <| [ C.lineTo  previousPoint ] ]

view : Model -> H.Html Msg
view ({ currentPoint, previousPoint, mode } as model) =
  H.div
    [ HA.style "display" "flex" ]
    [ C.toHtml
        (500, 500)
        [ M.onDown (.offsetPos >> StartAt)
        , M.onUp (.offsetPos >> EndAt)
        , M.onMove (.offsetPos >> MoveAt)
        , HA.style "border" "1px solid black"
        ]
        [ if model.mode == Cleared || model.mode == Blocked
          then C.clear (0, 0) 500 500
          else C.shapes
             [ CSL.lineCap CSL.RoundCap
             , CSL.lineWidth 3
             , CS.stroke (Color.rgb255 100 100 10)
             ] (renderables currentPoint previousPoint)
        ]
    , H.button [ HE.onClick Clear ][ H.text "Clear" ]
    , H.button [ HE.onClick Block ][ H.text "Block" ]
    ]

-- Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    StartAt point ->
        if model.mode == Blocked
        then model
        else { model | mode = Enabled, currentPoint = point, previousPoint = point }
    EndAt point ->
        if model.mode == Blocked
        then model
        else { model | mode = Disabled, currentPoint = point,previousPoint = point }
    MoveAt point ->
        if model.mode == Enabled
        then { model | currentPoint = point, previousPoint = model.currentPoint }
        else model
    Clear ->
        if model.mode == Blocked
        then model
        else { model | mode = Cleared }
    Block -> { model | mode = Blocked }

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
