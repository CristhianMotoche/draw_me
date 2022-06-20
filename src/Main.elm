module Main exposing (..)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Line as CSL
import Tuple as T
import Html as H
import Time exposing (posixToMillis, every)
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

type alias DrawingPointer =
  { previousMidPoint : C.Point, lastPoint : C.Point }

type Status =
  StandBy
  | Started
  | Joined

type alias Model =
  { currentPoint : C.Point
  , mode : PaintMode
  , pendingTicks : Int
  , pointer : Maybe DrawingPointer
  , status : Status
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point
  | LeaveAt C.Point
  | Clear
  | Restart
  | Tick Int
  | Start
  | Join

init : flags -> (Model, Cmd Msg)
init _ =
  ({ currentPoint = ( 0, 0 )
  , mode = Cleared
  , pendingTicks = 30
  , pointer = Nothing
  , status = StandBy
  }, Cmd.none)

-- View

-- TODO: Review: https://developpaper.com/canvas-advancement-how-to-draw-a-smooth-curve/
controlPoint ( x1, y1 ) ( x2, y2 ) =
    -- ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )
    ( (x2 + x1) / 2, (y2 + y1) / 2 )

renderables currentPoint {lastPoint} =
    let midPoint = controlPoint lastPoint currentPoint
    -- in [ C.path lastPoint [ C.lineTo currentPoint ] ]
    in [ C.path lastPoint [ C.quadraticCurveTo midPoint currentPoint ] ]

view : Model -> H.Html Msg
view model =
  case model.status of
    Started -> startView model
    _ -> standByView

standByView : H.Html Msg
standByView =
  H.div
  []
  [ H.div [] [ H.h1 [][H.text"Draw Me"] ]
  , H.div [] [ H.button [ M.onClick <| \_ -> Start ][H.text"Start"] ]
  , H.div [] [ H.button [ M.onClick <| \_ -> Join ][H.text"Join"] ]
  ]

startView : Model -> H.Html Msg
startView ({ currentPoint, mode } as model) =
  H.div
    [ HA.style "display" "flex"
    , HA.style "flex-direction" "column"
    , HA.style "width" "500px"
    , HA.style "height" "500px"
    , HA.style "border" "1px solid black"
    ]
    [
      C.toHtml
        (500, 500)
        [ M.onDown (.offsetPos >> StartAt)
        , M.onUp (.offsetPos >> EndAt)
        , M.onMove (.offsetPos >> MoveAt)
        , M.onLeave (.offsetPos >> LeaveAt)
        ]
        [ case (model.mode, model.pointer) of
                (Cleared, _) -> C.clear (0, 0) 500 500
                (_, Just pointer) ->
                  C.shapes
                     [ CSL.lineCap CSL.RoundCap
                     , CSL.lineWidth 3
                     , CS.stroke (Color.rgb255 100 100 10)
                     ] (renderables currentPoint pointer)
                _ -> C.shapes [] []
        ]
    , H.div
        []
        [ H.button [ HE.onClick Clear ][ H.text "Clear" ]
        , H.button [ HE.onClick Restart ][ H.text "Restart" ]
        , H.p [][ H.text <| "Pending ticks: " ++ String.fromInt model.pendingTicks ]
        ]
    ]

-- Update

noCmd : Model -> (Model, Cmd Msg)
noCmd m = (m, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartAt point -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Enabled, currentPoint = point, pointer = Just { lastPoint = point, previousMidPoint = point }}
    EndAt point -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Disabled, currentPoint = point, pointer = Nothing }
    MoveAt point -> noCmd <|
        case model.pointer of
          Just pointer -> drawPoint point pointer model
          _ -> model
    LeaveAt point -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Disabled, currentPoint = point, pointer = Nothing }
    Clear -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Cleared }
    Restart -> init ()
    Start -> noCmd { model | status = Started }
    Join -> noCmd model
    Tick milis ->
        if model.pendingTicks > 0
        then noCmd { model | pendingTicks = model.pendingTicks - 1 }
        else noCmd { model | pendingTicks = 0, mode = Blocked }


drawPoint newPoint { previousMidPoint, lastPoint } model =
  let newMidPoint = controlPoint lastPoint newPoint
  in { model | pointer = Just { previousMidPoint = newMidPoint, lastPoint = model.currentPoint }, currentPoint = newPoint}

toggle : PaintMode -> PaintMode
toggle m =
  case m of
    Enabled -> Disabled
    _ -> Enabled


-- Subs

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.mode == Blocked
    then Sub.none
    else every 1000 (posixToMillis >> Tick)

-- Main

main : Program () Model Msg
main =
  Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
