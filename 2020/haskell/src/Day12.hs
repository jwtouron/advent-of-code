module Day12 (test, solve) where

import AoCUtils
import Data.List

newtype Pos = Pos { unPos :: (Int, Int) } deriving Show

instance Semigroup Pos where
  Pos (a, b) <> Pos (a', b') = Pos (a + a', b + b')

instance Monoid Pos where
  mempty = Pos (0, 0)

data Dir = N | S | E | W deriving Show

data Action =
  Dir Dir Int | L Int | R Int | F Int
  deriving (Show)

parseAction :: String -> Action
parseAction s =
  case s of
    'N':cs -> Dir N $ read cs
    'S':cs -> Dir S $ read cs
    'E':cs -> Dir E $ read cs
    'W':cs -> Dir W $ read cs
    'L':cs -> L $ read cs
    'R':cs -> R $ read cs
    'F':cs -> F $ read cs
    _ -> error $  "invalid Action: " ++ s

data State =
  State { facing :: Dir
        , pos :: Pos
        , waypoint :: Pos
        } deriving Show

newState :: State
newState = State E mempty (Pos (1, 10))

example :: [Action]
example = map parseAction . lines $ "F10\n\
\N3\n\
\F7\n\
\R90\n\
\F11"

input :: IO [Action]
input = map parseAction . lines <$> readFile "input/day12.txt"

turnLeft :: State -> State
turnLeft (State N p w) = State W p w
turnLeft (State S p w) = State E p w
turnLeft (State E p w) = State N p w
turnLeft (State W p w) = State S p w

turnRight :: State -> State
turnRight (State N p w) = State E p w
turnRight (State S p w) = State W p w
turnRight (State E p w) = State S p w
turnRight (State W p w) = State N p w

stepPart1 :: State -> Action -> State
stepPart1 state action =
  case action of
    Dir N n -> State (facing state) (pos state <> Pos (n, 0)) (waypoint state)
    Dir S n -> State (facing state) (pos state <> Pos (-n, 0)) (waypoint state)
    Dir E n -> State (facing state) (pos state <> Pos (0, n)) (waypoint state)
    Dir W n -> State (facing state) (pos state <> Pos (0, -n)) (waypoint state)
    L 90 -> turnLeft state
    L 180 -> turnLeft . turnLeft $ state
    L 270 -> turnLeft . turnLeft . turnLeft $ state
    R 90 -> turnRight state
    R 180 -> turnRight . turnRight $ state
    R 270 -> turnRight . turnRight . turnRight $ state
    F n -> stepPart1 state (Dir (facing state) n)
    _ -> error $ "Unrecognized action: " ++ show action

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (a, b) (a', b') = abs (subtract a a') + abs (subtract b b')

rotateWPLeft :: State -> State
rotateWPLeft (State f p (Pos (a, b))) = State f p (Pos (b, -a))

rotateWPRight :: State -> State
rotateWPRight (State f p (Pos (a, b))) = State f p (Pos (-b, a))

stepPart2 :: State -> Action -> State
stepPart2 state@(State facing pos wp) action =
  case action of
    Dir N n -> State facing pos (wp <> Pos (n, 0))
    Dir S n -> State facing pos (wp <> Pos (-n, 0))
    Dir E n -> State facing pos (wp <> Pos (0, n))
    Dir W n -> State facing pos (wp <> Pos (0, -n))
    L 90 -> rotateWPLeft state
    L 180 -> rotateWPLeft . rotateWPLeft $ state
    L 270 -> rotateWPLeft . rotateWPLeft . rotateWPLeft $ state
    R 90 -> rotateWPRight state
    R 180 -> rotateWPRight . rotateWPRight $ state
    R 270 -> rotateWPRight . rotateWPRight . rotateWPRight $ state
    F n -> State facing (pos <> Pos (n * fst (unPos wp), n * snd (unPos wp))) wp
    _ -> error $ "Unrecognized action: " ++ show action

solve' :: (State -> Action -> State) -> [Action] -> Int
solve' step = manhattanDistance (0, 0) . unPos . pos . foldl' step newState

part1, part2 :: [Action] -> Int
part1 = solve' stepPart1
part2 = solve' stepPart2

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 25 (part1 example)
         , mkTestCase "part1 input" 439 (part1 input')
         , mkTestCase "part2 example" 286 (part2 example)
         , mkTestCase "part2 input'" 12385 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
