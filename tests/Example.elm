module Example exposing (..)

import Test exposing (..)
import Main exposing (Eff(..), Model, Msg(..), init, view, update)
import Test.Html.Selector exposing (text)
import ProgramTest as PT
import Debug
import SimulatedEffect.Cmd as SCmd

simulatedCmd : Eff -> PT.SimulatedEffect Msg
simulatedCmd eff =
  case eff of
    NoEff -> SCmd.none
    GenWord -> SCmd.none

simulatedSub : Model -> PT.SimulatedSub Msg
simulatedSub _ = Debug.todo "wait for simulated Time.every"

start : PT.ProgramTest Model Msg Eff
start =
  PT.createElement
  { init = init
  , update = update
  , view = view
  }
  |> PT.withSimulatedEffects simulatedCmd
  |> PT.withSimulatedSubscriptions simulatedSub
  |> PT.start ()

second = 1000

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
          |> PT.update (GeneratedWord "Hat")
          |> PT.expectViewHas
             [ text "Pending ticks: 30" ]
    , describe "and clicks on leave"
      [ test "shows Draw me title again" <|
          \_ ->
             start
             |> PT.clickButton "Start"
             |> PT.update (GeneratedWord "Hat")
             |> PT.clickButton "Leave"
             |> PT.expectViewHas
                [ text "Draw Me"
                , text "Start"
                ]
      ]
    , skip <| describe "and passes 60 seconds"
      [ test "shows Game Over message" <|
          \_ ->
             start
             |> PT.clickButton "Start"
             |> PT.update (GeneratedWord "Hat")
             |> PT.advanceTime (60 * second)
             |> PT.expectViewHas
                [ text "Game over" ]
      ]
    , test "shows generated word" <|
        \_ ->
           start
           |> PT.clickButton "Start"
           |> PT.update (GeneratedWord "Hat")
           |> PT.expectViewHas
              [ text "Hat" ]
    ]
  ]
