module Canvas exposing (..)

import Browser
import Html as H

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
    []
    [ H.text <| "X" ++ String.fromFloat model.currentPoint.x
    , H.text <| "Y" ++ String.fromFloat model.currentPoint.y
    ]

update : Msg -> Model -> Model
update _ model = model

main : Program () Model Msg
main =
  Browser.sandbox
  { init = init
  , view = view
  , update = update
  }
