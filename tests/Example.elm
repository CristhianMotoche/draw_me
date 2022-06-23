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
  describe "Start Page"
  [ test "shows elems in page" <|
      \_ ->
        start |>
          PT.expectViewHas
          [ text "Draw Me"
          , text "Start"
          , text "Join"
          ]
  , describe "when clicks on start"
    [ test "shows canvas" <|
        \_ ->
          start
          |> PT.clickButton "Start"
          |> PT.expectViewHas
             [ text "Pending ticks: 30" ]
    , describe "and clicks on leave"
      [ test "shows Draw me title again" <|
          \_ ->
             start
             |> PT.clickButton "Start"
             |> PT.clickButton "Leave"
             |> PT.expectViewHas
                [ text "Draw Me"
                , text "Start"
                ]
      ]
    ]
  ]
