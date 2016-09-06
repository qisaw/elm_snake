import Time exposing (..)
import Keyboard
import Color exposing (..)
import List exposing (map, head, length, take)
import Set exposing (fromList, size)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window exposing (dimensions)

type alias BoundingSquare = { tLeft: CoOrd, tRight: CoOrd, bLeft: CoOrd, bRight: CoOrd }
type alias CoOrd = { x : Float, y: Float }
type alias Dimensions = (Int, Int)
type alias Game = { snake : Snake , finished: Bool }
type alias Snake = { direction : Action, segments : List CoOrd }
type alias ActionList = List Action
type alias KeyboardInput = { x : Int, y: Int }
type alias Directions = { direction : Action, list : ActionList }

tileSize : Float
tileSize = 20

padding : Float
padding = 2

type Action = Up | Down | Left | Right | None

initSingleCoOrd : CoOrd
initSingleCoOrd = {x=tileSize/2, y=0}
initCoOrds : Float -> List CoOrd
initCoOrds end = List.map (\i -> {x = -i * tileSize + initSingleCoOrd.x, y = initSingleCoOrd.y }) [0..end]
snake0 : Snake
snake0 = { direction = None, segments = initCoOrds 20 }
game0 : Game
game0 = { snake = snake0, finished = False}

createAction : KeyboardInput -> Action
createAction m =
  if m.x == 1 then
    Right
  else if m.x == -1 then
    Left
  else if m.y == 1 then
    Up
  else if m.y == -1 then
    Down
  else
    None

createNewSegment : Maybe CoOrd -> Action -> CoOrd
createNewSegment c a =
  case c of
    Nothing -> initSingleCoOrd
    Just m ->
      case a of
        Up -> { m | y = m.y + tileSize}
        Down -> { m | y = m.y - tileSize}
        Left -> { m | x = m.x - tileSize}
        Right -> { m | x = m.x + tileSize}
        None -> m

getSnakeSegments : List CoOrd -> Action -> List CoOrd
getSnakeSegments list action =
  createNewSegment (List.head list) action :: List.take (length list - 1) list

isSnakeAlive : Snake -> Bool
isSnakeAlive {direction, segments} =
  Set.size (fromList (map (\{x,y} -> (x, y)) segments)) == length segments


update : Action -> Game -> Game
update a g =
  if g.finished then g
  else
    let
      s = g.snake
      s' =
        case a of
          Up -> { s | direction = Up}
          Down -> { s | direction = Down}
          Left -> { s | direction = Left}
          Right -> { s | direction = Right}
          None ->
            case s.direction of
              None -> s
              x -> { s | segments = getSnakeSegments s.segments x}
    in
      { g | snake = s', finished = not (isSnakeAlive s') }


createSnakeSegment : CoOrd -> Form
createSnakeSegment c =
    square (tileSize - padding) |> filled black |> move (c.x, c.y)

createSnake : Snake -> List Form
createSnake s =
  List.map createSnakeSegment s.segments

drawGame : Game -> Dimensions -> Element
drawGame g (w,h) =
  let
    floatW = toFloat w
    floatH = toFloat h
  in
    if g.finished then show "GAME OVER"
    else
      collage w h (
        filled lightGrey (rect floatW floatH)
        :: outlined (solid grey) (rect floatW floatH)
        :: createSnake g.snake)

--Signals
-- have to map signal of time to signal of action
timeActionSignal : Signal Action
timeActionSignal = Signal.map (always None) (fps 8)

actionSignal : Signal Action
actionSignal = Signal.map createAction Keyboard.arrows

allSignal : Signal Action
allSignal = Signal.merge actionSignal timeActionSignal

gameSignal : Signal Game
gameSignal = Signal.foldp update game0 allSignal

main : Signal Element
main =
  Signal.map2 drawGame gameSignal Window.dimensions
