module Main exposing (..)

import Browser
import Canvas as C
import Tuple as T
import Html as H
import Html.Attributes as HA
import Html.Events.Extra.Mouse as M

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
view model =
  C.toHtml (300, 300)
       [ HA.style "border" "1px solid black"  ]
       [ C.shapes [] [ C.rect (0, 0) 100 50 ] ]
--    [ M.onDown <|
--        \ev -> Paint { x = T.first ev.clientPos, y = T.second ev.clientPos },
--      HA.width 200,
--      HA.height 100
--    ]
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
