module Canvas exposing (..)

import Browser
import Tuple as T
import Html as H
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
  H.div
    [ M.onDown <| \ev -> Paint { x = T.first ev.clientPos, y = T.second ev.clientPos } ]
    [ H.text <| "X" ++ String.fromFloat model.currentPoint.x
    , H.text <| "Y" ++ String.fromFloat model.currentPoint.y
    ]

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
