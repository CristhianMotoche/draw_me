port module Main exposing (..)

import Browser
import Canvas as C
import Canvas.Settings as CS
import Canvas.Settings.Line as CSL
import Html as H
import Time exposing (posixToMillis, every)
import Html.Attributes as HA
import Html.Events.Extra.Mouse as M
import Html.Events as HE
import Random as R
import Color

-- JavaScript usage: app.ports.websocketIn.send(response);
port websocketIn : (String -> msg) -> Sub msg
-- JavaScript usage: app.ports.websocketOut.subscribe(handler);
port websocketOut : String -> Cmd msg

type Eff =
   NoEff | GenWord | WSOut String


words : List String
words =
  [ "Dog"
  , "Cat"
  , "Computer"
  , "Television"
  , "Cell phone"
  ]

run : Eff -> Cmd Msg
run eff =
  case eff of
    NoEff -> Cmd.none
    GenWord -> R.generate GeneratedWord <| R.uniform "Hat" words
    WSOut value -> Cmd.map (\_ -> Outgoing value) (websocketOut value)

type PaintMode
  = Enabled
  | Disabled
  | Cleared
  | Blocked

type alias DrawingPointer =
  { previousMidPoint : C.Point, lastPoint : C.Point }

type Status =
  StandBy
  | Started
  | Joined

type alias Model =
  { currentPoint : C.Point
  , mode : PaintMode
  , pendingTicks : Int
  , pointer : Maybe DrawingPointer
  , status : Status
  , word : Maybe String
  }

type Msg
  = StartAt C.Point
  | MoveAt C.Point
  | EndAt C.Point
  | LeaveAt C.Point
  | GeneratedWord String
  | Incoming String
  | Outgoing String
  | Clear
  | Leave
  | Tick Int
  | Start
  | Join

init : flags -> (Model, Eff)
init _ =
  ({ currentPoint = ( 0, 0 )
  , mode = Cleared
  , pendingTicks = 100
  , pointer = Nothing
  , status = StandBy
  , word = Nothing
  }, NoEff)

-- View

-- TODO: Review: https://developpaper.com/canvas-advancement-how-to-draw-a-smooth-curve/
controlPoint ( x1, y1 ) ( x2, y2 ) =
    -- ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )
    ( (x2 + x1) / 2, (y2 + y1) / 2 )

renderables currentPoint {lastPoint} =
    let midPoint = controlPoint lastPoint currentPoint
    -- in [ C.path lastPoint [ C.lineTo currentPoint ] ]
    in [ C.path lastPoint [ C.quadraticCurveTo midPoint currentPoint ] ]

view : Model -> H.Html Msg
view model =
  case (model.word, model.status) of
    (Just word, Started) -> startView word model
    (_, Joined) -> joinView model
    _ -> standByView

standByView : H.Html Msg
standByView =
  H.div
  []
  [ H.div [] [ H.h1 [][ H.text "Draw Me" ] ]
  , H.div [] [ H.button [ HE.onClick Start ][H.text"Start"] ]
  , H.div [] [ H.button [ HE.onClick Join ][H.text"Join"] ]
  ]

startView : String -> Model -> H.Html Msg
startView word ({ currentPoint } as model) =
  H.div
    [ HA.style "display" "flex"
    , HA.style "flex-direction" "column"
    , HA.style "width" "500px"
    , HA.style "height" "500px"
    , HA.style "border" "1px solid black"
    ]
    [ C.toHtml
        (500, 500)
        [ M.onDown (.offsetPos >> StartAt)
        , M.onUp (.offsetPos >> EndAt)
        , M.onMove (.offsetPos >> MoveAt)
        , M.onLeave (.offsetPos >> LeaveAt)
        ]
        [ case (model.mode, model.pointer) of
                (Cleared, _) -> C.clear (0, 0) 500 500
                (_, Just pointer) ->
                  C.shapes
                     [ CSL.lineCap CSL.RoundCap
                     , CSL.lineWidth 3
                     , CS.stroke (Color.rgb255 100 100 10)
                     ] (renderables currentPoint pointer)
                _ -> C.shapes [] []
        ]
    , H.div
      []
      [ H.span [][ H.text "Draw me a: " ]
      , H.span [][ H.text word ]
      ]
    , H.div
        []
        [ H.button [ HE.onClick Clear ][ H.text "Clear" ]
        , H.button [ HE.onClick Leave ][ H.text "Leave" ]
        , H.p [][ H.text <| "Pending ticks: " ++ String.fromInt model.pendingTicks ]
        ]
    ]

joinView : Model -> H.Html Msg
joinView ({ currentPoint } as model) =
  H.div
    [ HA.style "display" "flex"
    , HA.style "flex-direction" "column"
    , HA.style "width" "500px"
    , HA.style "height" "500px"
    , HA.style "border" "1px solid black"
    ]
    [ C.toHtml
        (500, 500)
        []
        [ case (model.mode, model.pointer) of
                (_, Just pointer) ->
                  C.shapes
                     [ CSL.lineCap CSL.RoundCap
                     , CSL.lineWidth 3
                     , CS.stroke (Color.rgb255 100 100 10)
                     ] (renderables currentPoint pointer)
                _ -> C.shapes [] []
        ]
    , H.div
      []
      [ H.span [][ H.text "What is that?" ]
      ]
    , H.div
        []
        [ H.button [ HE.onClick Leave ][ H.text "Leave" ]
        , H.p [][ H.text <| "Pending ticks: " ++ String.fromInt model.pendingTicks ]
        ]
    ]

-- Update

noCmd : Model -> (Model, Eff)
noCmd m = (m, NoEff)

pointToString : C.Point -> String
pointToString (x, y) = String.fromFloat x ++ "," ++ String.fromFloat y

type Step
  = StartStep
  | Move
  | End

stepToString : Step -> String
stepToString step =
  case step of
    StartStep -> "S"
    Move -> "M"
    End -> "E"

stepFromStr : String -> Maybe Step
stepFromStr step =
  case step of
    "S" -> Just StartStep
    "M" -> Just Move
    "E" -> Just End
    _ -> Nothing

pointWithModeToString : Step -> C.Point -> String
pointWithModeToString step point =
  stepToString step ++ "," ++ pointToString point


pointWithModeFromString : String -> Maybe (Step, C.Point)
pointWithModeFromString str =
  case String.split "," str of
      sStr :: rest ->
        case (stepFromStr sStr, pointFromString (String.join "," rest)) of
            (Just step, Just point) -> Just (step, point)
            _ -> Nothing
      _ -> Nothing

pointFromString : String -> Maybe C.Point
pointFromString strPoint =
  case String.split "," strPoint of
    xStr :: yStr :: [] ->
      case (String.toFloat xStr, String.toFloat yStr) of
          (Just x, Just y) -> Just (x, y)
          _ -> Nothing
    _ -> Nothing

update : Msg -> Model -> (Model, Eff)
update msg model =
  case msg of
    StartAt point ->
        if model.mode == Blocked
        then noCmd model
        else ({ model |
                mode = Enabled,
                currentPoint = point,
                pointer = Just { lastPoint = point, previousMidPoint = point }}
              , WSOut <| pointWithModeToString StartStep point
              )
    EndAt point ->
        if model.mode == Blocked
        then  noCmd model
        else ({ model |
                  mode = Disabled,
                  currentPoint = point, pointer = Nothing}
             , WSOut <| pointWithModeToString End point )
    MoveAt point ->
        case model.pointer of
          Just pointer ->
            (drawPoint point pointer model
            , WSOut <| pointWithModeToString Move point)
          _ -> noCmd model
    LeaveAt point -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Disabled, currentPoint = point, pointer = Nothing }
    Clear -> noCmd <|
        if model.mode == Blocked
        then model
        else { model | mode = Cleared }
    Leave -> init ()
    Start -> ({ model | status = Started }, GenWord)
    Join -> ({ model | status = Joined }, WSOut "guesser")
    Tick _ ->
        if model.pendingTicks > 0
        then noCmd { model | pendingTicks = model.pendingTicks - 1 }
        else noCmd { model | pendingTicks = 0, mode = Blocked }
    GeneratedWord word ->
      ({ model | word = Just word }, WSOut "drawer")
    Incoming value ->
      case pointWithModeFromString <| Debug.log "V" value of
          Just (StartStep, point) ->
            ({ model
              | currentPoint = point
              , pointer = Just { lastPoint = point, previousMidPoint = point }}
              , NoEff
              )
          Just (End, point) ->
            ({ model
              | currentPoint = point
              , pointer = Nothing }
              , NoEff
              )
          Just (Move, point) ->
            case model.pointer of
              Just pointer ->
                (drawPoint point pointer model
                , NoEff)
              _ -> noCmd model
          _ -> noCmd model
    Outgoing value ->
      Debug.log value
      noCmd model

drawPoint : C.Point -> DrawingPointer -> Model -> Model
drawPoint newPoint { lastPoint } model =
  let newMidPoint = controlPoint lastPoint newPoint
  in { model
     | pointer =
        Just
          { previousMidPoint = newMidPoint
          , lastPoint = model.currentPoint
          }
     , currentPoint = newPoint
     }

toggle : PaintMode -> PaintMode
toggle m =
  case m of
    Enabled -> Disabled
    _ -> Enabled


-- Subs

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ if model.mode == Blocked
    then Sub.none
    else every 1000 (posixToMillis >> Tick)
  , websocketIn Incoming
  ]
-- Main

main : Program () Model Msg
main =
  Browser.element
  { init = \flags -> init flags |> Tuple.mapSecond run
  , view = view
  , update = \msg model -> update msg model |> Tuple.mapSecond run
  , subscriptions = subscriptions
  }
