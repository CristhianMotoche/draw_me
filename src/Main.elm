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

type alias Model =
  { previousPoint : C.Point
  , currentPoint : C.Point
  , points : List C.Point
  , mode : PaintMode
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point
  | Clear

init : Model
init =
  { currentPoint = ( 0, 0 )
  , previousPoint = ( 0, 0 )
  , points = []
  , mode = Disabled
  }

-- View

-- renderables ((cx, cy) as currentPoint) ((px, py) as previousPoint) =
--     [ C.path currentPoint <| [ C.lineTo previousPoint ] ]

renderables points =
    case points of
        [] -> []
        (x :: []) -> []
        (currentPoint :: previousPoint :: xs) ->
            [ C.path currentPoint <| [ C.lineTo previousPoint ] ]
            ++ renderables xs

-- renderables  =
--     [ C.path currentPoint <| [ C.lineTo previousPoint ] ]

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
        [ if model.mode == Cleared
          then C.clear (0, 0) 500 500
          else C.shapes
             [ CSL.lineCap CSL.RoundCap
             , CSL.lineWidth 3
             , CS.stroke (Color.rgb255 100 100 10)
             ] (renderables model.points)
        ]
    , H.button [ HE.onClick Clear ][ H.text "Clear" ]
    ]

-- Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    StartAt point ->
        { model |
                  mode = Enabled
                , points = List.append [point] model.points
        }
    EndAt point ->
        { model |
                  mode = Disabled
                , points = []
        }
    MoveAt point ->
        if model.mode == Enabled
        then { model | points = List.append [point] model.points }
        else model
    Clear -> { model | mode = Cleared }

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
