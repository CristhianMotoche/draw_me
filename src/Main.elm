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

type alias Model =
  { previousPoint : C.Point
  , currentPoint : C.Point
  , mode : PaintMode
  , pendingTicks : Int
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point
  | Clear
  | Restart
  | Tick Int

init : flags -> (Model, Cmd Msg)
init _ =
  ({ currentPoint = ( 0, 0 )
  , previousPoint = ( 0, 0 )
  , mode = Cleared
  , pendingTicks = 15
  }, Cmd.none)

-- View

renderables ((cx, cy) as currentPoint) ((px, py) as previousPoint) =
    [ C.path currentPoint <| [ C.lineTo  previousPoint ] ]

view : Model -> H.Html Msg
view ({ currentPoint, previousPoint, mode } as model) =
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
        ]
        [ if model.mode == Cleared
          then C.clear (0, 0) 500 500
          else C.shapes
             [ CSL.lineCap CSL.RoundCap
             , CSL.lineWidth 3
             , CS.stroke (Color.rgb255 100 100 10)
             ] (renderables currentPoint previousPoint)
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
        else { model | mode = Enabled, currentPoint = point, previousPoint = point }
    EndAt point -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Disabled, currentPoint = point,previousPoint = point }
    MoveAt point -> noCmd <|
        if model.mode == Enabled
        then { model | currentPoint = point, previousPoint = model.currentPoint }
        else model
    Clear -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Cleared }
    Restart -> init ()
    Tick milis ->
        if model.pendingTicks > 0
        then noCmd { model | pendingTicks = model.pendingTicks - 1 }
        else noCmd { model | pendingTicks = 0, mode = Blocked }

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
