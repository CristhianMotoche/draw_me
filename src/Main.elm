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

type alias Model =
  { currentPoint : Point
  }

type alias Point =
  { x: Float
  , y: Float
  }

type Msg = Paint Point

init : Model
init = { currentPoint = { x = 0, y = 0 } }

view : Model -> H.Html Msg
view { currentPoint } =
  H.div
    [ HA.style "display" "flex" ]
    [ C.toHtml
        (300, 300)
        [ M.onDown <|
            \ev -> Paint { x = T.first ev.offsetPos, y = T.second ev.offsetPos },
          HA.style "border" "1px solid black"  ]
        [ C.shapes [ CS.fill (Color.rgb255 100 100 10) ] [ C.rect (currentPoint.x, currentPoint.y) 5 5 ] ]
    ]
  --    [ H.text <| "X" ++ String.fromFloat model.currentPoint.x
  --    , H.text <| "Y" ++ String.fromFloat model.currentPoint.y
  --    ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    Paint point -> { model | currentPoint = point }


main : Program () Model Msg
main =
  Browser.sandbox
  { init = init
  , view = view
  , update = update
  }
