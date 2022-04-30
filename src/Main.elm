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
  { currentPoint : C.Point
  , mode : PaintMode
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point

init : Model
init =
  { currentPoint = ( 0, 0 )
  , mode = Disabled
  }

-- View

view : Model -> H.Html Msg
view { currentPoint, mode } =
  H.div
    [ HA.style "display" "flex" ]
    [ C.toHtml
        (300, 300)
        [ M.onDown (.offsetPos >> StartAt)
        , M.onUp (.offsetPos >> EndAt)
        , M.onMove (.offsetPos >> MoveAt)
        , HA.style "border" "1px solid black"  ]
        [ C.shapes [ CS.fill (Color.rgb255 100 100 10) ] [ C.rect currentPoint 5 5 ] ]
    ]

-- Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    StartAt point -> { model | mode = Enabled }
    EndAt point -> { model | mode = Disabled }
    MoveAt point ->
        if model.mode == Enabled
        then { model | currentPoint = point }
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
