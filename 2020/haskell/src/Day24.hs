module Day24 (test, solve) where

import AoCUtils
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set

type Tile = (Int, Int)
type TileColorMap = Map Tile Color

data Color = White | Black deriving (Eq, Show)
data State =
  State { currentTile :: Tile
        , tileColors :: TileColorMap
        } deriving (Show)

newState :: State
newState = State (0, 0) (Map.singleton (0, 0) White)

data Move = E | SE | SW | W | NW | NE deriving (Show)

parseLine :: String -> [Move]
parseLine "" = []
parseLine ('e':cs) = E : parseLine cs
parseLine ('s':'e':cs) = SE : parseLine cs
parseLine ('s':'w':cs) = SW : parseLine cs
parseLine ('w':cs) = W : parseLine cs
parseLine ('n':'w':cs) = NW : parseLine cs
parseLine ('n':'e':cs) = NE : parseLine cs
parseLine _ = error "parseLine: shouldn't happen"

flipTile :: Tile -> TileColorMap -> TileColorMap
flipTile = Map.alter f
  where f Nothing = Just Black
        f (Just White) = Just Black
        f (Just Black) = Just White

move :: Move -> State -> State
move move state@(State (x, y) _) = state { currentTile = tile' }
  where
    tile' =
      case move of
        E  -> (x + 1, y)
        SE -> (x + 1, y - 1)
        SW -> (x, y - 1)
        W  -> (x - 1, y)
        NW -> (x - 1, y + 1)
        NE -> (x, y + 1)

moveFlip :: [Move] -> State -> State
moveFlip moves state = state' { tileColors = flipTile tile' cs' }
  where state'@(State tile' cs') = foldl' (flip move) state moves

resetCurrentTile :: State -> State
resetCurrentTile state = state { currentTile = (0, 0) }

installNewTiles :: [[Move]] -> State
installNewTiles = foldl' ((resetCurrentTile .) . flip moveFlip) newState

part1 :: [[Move]] -> Int
part1 =
  Map.size
  . Map.filter (== Black)
  . tileColors
  . installNewTiles

allNeighbors :: Tile -> [Tile]
allNeighbors (x, y) =
  [(x + 1, y), (x + 1, y - 1), (x, y - 1), (x, y + 1), (x - 1, y), (x - 1, y + 1)]

blackNeighbors :: TileColorMap -> Tile -> [Tile]
blackNeighbors tileColors tile =
  [n | n <- allNeighbors tile, let c = Map.findWithDefault White n tileColors, c == Black]

borderNeighbors :: TileColorMap -> Tile -> [Tile]
borderNeighbors tileColors tile =
  [n | n <- allNeighbors tile, Map.notMember n tileColors]

stepDay :: TileColorMap -> TileColorMap
stepDay tileColors = foldl' f Map.empty $ Map.keys tileColors <> Set.toList bns
  where
    bns = foldl' f Set.empty $ Map.keys tileColors
      where f s t = foldl' (flip Set.insert) s $ borderNeighbors tileColors t
    f m t =
      let bns = blackNeighbors tileColors t
      in case Map.findWithDefault White t tileColors of
           Black | null bns || length bns > 2 -> Map.insert t White m
           White | length bns == 2 -> Map.insert t Black m
           c -> Map.insert t c m

part2 :: [[Move]] -> Int
part2 =
  Map.size
  . Map.filter (== Black)
  . (!! 100)
  . iterate stepDay
  . tileColors
  . installNewTiles

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 10 (part1 example)
         , mkTestCase "part1 input" 488 (part1 input')
         , mkTestCase "part2 example" 2208 (part2 example)
         , mkTestCase "part2 input" 4118 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  print $ part1 input'
  print $ part2 input'

example :: [[Move]]
example = map parseLine $ lines s
  where
    s = "sesenwnenenewseeswwswswwnenewsewsw\n\
\neeenesenwnwwswnenewnwwsewnenwseswesw\n\
\seswneswswsenwwnwse\n\
\nwnwneseeswswnenewneswwnewseswneseene\n\
\swweswneswnenwsewnwneneseenw\n\
\eesenwseswswnenwswnwnwsewwnwsene\n\
\sewnenenenesenwsewnenwwwse\n\
\wenwwweseeeweswwwnwwe\n\
\wsweesenenewnwwnwsenewsenwwsesesenwne\n\
\neeswseenwwswnwswswnw\n\
\nenwswwsewswnenenewsenwsenwnesesenew\n\
\enewnwewneswsewnwswenweswnenwsenwsw\n\
\sweneswneswneneenwnewenewwneswswnese\n\
\swwesenesewenwneswnwwneseswwne\n\
\enesenwswwswneneswsenwnewswseenwsese\n\
\wnwnesenesenenwwnenwsewesewsesesew\n\
\nenewswnwewswnenesenwnesewesw\n\
\eneswnwswnwsenenwnwnwwseeswneewsenese\n\
\neswnwewnwnwseenwseesewsenwsweewe\n\
\wseweeenwnesenwwwswnew\n"

input :: IO [[Move]]
input = map parseLine . lines <$> readFile "input/day24.txt"
