import Time exposing (..)
import Html exposing (div, button, text, Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)


-- Model and Action data types

type alias Model = { lastUpdatedTime : Time, displayedTime : Float, running : Bool, lapTimes : List Float }

model0 = { lastUpdatedTime = 0, displayedTime = 0, running = False, lapTimes = [0] }

type Action = Update Time | Start Time | Stop Time | Reset Time | RecordLap Time

type ButtonAction = StartButton | StopButton | ResetButton | RecordLapButton

-- Model update logic

updateDisplayedTime : Model -> Time -> Model
updateDisplayedTime m t = { m | displayedTime = m.displayedTime + t - m.lastUpdatedTime, lastUpdatedTime = t }

update : Action -> Model -> Model
update a m =
  let
    displayTimeUpdater = updateDisplayedTime m
  in
    case a of
      Update currentTime ->
        if not m.running then m
        else displayTimeUpdater currentTime
      Start startTime -> { m | lastUpdatedTime = startTime, running = True  }
      Stop stopTime ->
        let
          m' = displayTimeUpdater stopTime
        in
          { m' | running = False  }
      Reset resetTime -> {m | lastUpdatedTime = resetTime, displayedTime = 0, lapTimes = [0] }
      RecordLap lapTime ->
        if not m.running then m else
          let
            m' = displayTimeUpdater lapTime
          in
            {m' | lapTimes = m.displayedTime :: m.lapTimes }


-- View
startButtonLabel : Model -> String
startButtonLabel m = if m.running then "pause" else (if m.displayedTime == 0 then "start" else "continue")

view : Model -> Html
view m =
  div []
    ([ button [ onClick buttonsMailbox.address <| if m.running then StopButton else StartButton] [ text <| startButtonLabel m ]
    , div [] [ text <| toString m.displayedTime ]
    , button [ onClick buttonsMailbox.address ResetButton ] [ text "reset" ]
    , button [ onClick buttonsMailbox.address RecordLapButton, disabled <| not m.running ] [ text "lap" ]
    ] ++ (List.map (\ t -> div [] [ text <| toString t]) <| firstDifferences (List.reverse m.lapTimes )))


-- Signal wiring

buttonsMailbox : Signal.Mailbox ButtonAction
buttonsMailbox = Signal.mailbox StartButton

time : Signal.Signal Time
time = every millisecond

tickSignal : Signal.Signal Action
tickSignal = Signal.map Update time

allButtonsSignal : Signal.Signal ButtonAction
allButtonsSignal = buttonsMailbox.signal

buttonActionTime : Signal.Signal Time
buttonActionTime = Signal.sampleOn allButtonsSignal time

tagTimeOntoButtonAction : Time -> ButtonAction -> Action
tagTimeOntoButtonAction time button =
  case button of
    StartButton -> Start time
    StopButton  -> Stop  time
    ResetButton -> Reset time
    RecordLapButton -> RecordLap time

timedButtonActionSignal : Signal.Signal Action
timedButtonActionSignal = Signal.map2 tagTimeOntoButtonAction buttonActionTime allButtonsSignal

actionSignal : Signal.Signal Action
actionSignal = Signal.merge tickSignal timedButtonActionSignal

model : Signal.Signal Model
model = Signal.foldp update model0 actionSignal

main : Signal.Signal Html
main = Signal.map view model


-- Utilities

firstDifferences : List Float -> List Float
firstDifferences xs = (List.map2 (-)) (Maybe.withDefault [] (List.tail xs)) xs
