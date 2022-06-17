module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (Model, Msg, init, view, update, subscriptions)
import Test.Html.Selector exposing (text)
import ProgramTest as PT

start : PT.ProgramTest Model Msg (Cmd Msg)
start =
  PT.createElement
  { init = init
  , update = update
  , view = view
  }
  |> PT.start ()

startPage : Test
startPage =
  test "Start Page" <|
    \_ ->
        start
        |> PT.expectViewHas
           [ text "Draw Me"
           , text "Start"
           , text "Join"
           ]
