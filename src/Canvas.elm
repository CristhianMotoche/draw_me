module Canvas exposing (..)

import Browser
import Html as H

type alias Model =
  { pos : Int
  }

type Msg = Move

init : Model
init = { pos = 0 }

view : Model -> H.Html Msg
view _ = H.div [] []

update : Msg -> Model -> Model
update _ model = model

main : Program () Model Msg
main =
  Browser.sandbox
  { init = init
  , view = view
  , update = update
  }
