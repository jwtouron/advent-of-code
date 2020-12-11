{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 (test, solve) where

import AoCUtils
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List
import GHC.Generics (Generic)

data Cell =
  Cell { row :: {-# UNPACK #-} !Int
       , col :: {-# UNPACK #-} !Int
       } deriving (Eq, Ord, Show, Generic, Hashable)

data SeatLayout =
  SeatLayout { cells :: HashMap Cell Char
             , width :: Int
             , height :: Int
             } deriving (Eq, Show)

seatLayoutFromLines :: [String] -> SeatLayout
seatLayoutFromLines lines = SeatLayout cells (length $ lines !! 0) (length lines)
  where cells = foldl' f Map.empty (zip [0..] lines)
          where f m (row, line) = foldl' g m (zip [0..] line)
                  where g m (col, char) = Map.insert (Cell row col) char m

example :: SeatLayout
example = seatLayoutFromLines lines
  where
    lines = [ "L.LL.LL.LL"
            , "LLLLLLL.LL"
            , "L.L.L..L.."
            , "LLLL.LL.LL"
            , "L.LL.LL.LL"
            , "L.LLLLL.LL"
            , "..L.L....."
            , "LLLLLLLLLL"
            , "L.LLLLLL.L"
            , "L.LLLLL.LL"
            ]

part2Example1 :: SeatLayout
part2Example1 = seatLayoutFromLines lines'
  where
    lines' = lines ".......#.\n\
\...#.....\n\
\.#.......\n\
\.........\n\
\..#L....#\n\
\....#....\n\
\.........\n\
\#........\n\
\...#.....\n"

part2Example2 :: SeatLayout
part2Example2 = seatLayoutFromLines lines'
  where
    lines' = lines ".............\n\
\.L.L.#.#.#.#.\n\
\............."

part2Example3 :: SeatLayout
part2Example3 = seatLayoutFromLines lines'
  where
    lines' = lines ".##.##.\n\
\#.#.#.#\n\
\##...##\n\
\...L...\n\
\##...##\n\
\#.#.#.#\n\
\.##.##."

input :: IO SeatLayout
input = seatLayoutFromLines . lines <$> readFile "input/day11.txt"

immediateNeighbors :: Cell -> SeatLayout -> [Cell]
immediateNeighbors (Cell row col) (SeatLayout cells _ _) = filter (`Map.member` cells) allCells
  where
    allCells = [Cell (row + r') (col + c') | r' <- [1, (-1), 0]
                                           , c' <- [1, (-1), 0]
                                           , not (r' == 0 && c' == 0)]

extensiveNeighbors :: Cell -> SeatLayout -> [Cell]
extensiveNeighbors (Cell row col) sl@SeatLayout{..} =
  concatMap (f . dropWhile (not . isSeat)) [ up, upRight, right, downRight, down, downLeft, left, upLeft ]
  where
    f [] = []
    f (x:_) = [x]
    up = [Cell r col | r <- ups]
    upRight = [Cell r c | (r, c) <- zip ups rights]
    right = [Cell row c | c <- rights]
    downRight = [Cell r c | (r, c) <- zip downs rights]
    down = [Cell r col | r <- downs]
    downLeft = [Cell r c | (r, c) <- zip downs lefts]
    left = [Cell row c | c <- lefts]
    upLeft = [Cell r c | (r, c) <- zip ups lefts]
    ups = [pred row, pred (pred row) .. 0]
    rights = [succ col..pred width]
    downs = [succ row .. pred height]
    lefts = [pred col, pred (pred col) .. 0]
    isSeat cell = cellVal sl cell == '#' || cellVal sl cell == 'L'

cellVal :: SeatLayout -> Cell -> Char
cellVal sl cell = cells sl Map.! cell

step :: Int -> (Cell -> SeatLayout -> [Cell]) -> SeatLayout -> SeatLayout
step threshold neighbors sl@SeatLayout{..} = sl { cells = cells' }
  where
    cells' = foldl' f Map.empty [Cell row col | row <- [0..pred height]
                                              , col <- [0.. pred width]]
      where
        f m cell
          | cellVal sl cell == 'L' &&
            null (filter ((== '#') . cellVal sl) neighbors') =
              insert cell '#'
          | cellVal sl cell == '#' &&
            ((>= threshold) . length . filter ((== '#') . cellVal sl) $ neighbors') =
              insert cell 'L'
          | otherwise = insert cell (cellVal sl cell)
          where
            neighbors' = neighbors cell sl
            insert k v = Map.insert k v m

drawSeatLayout :: SeatLayout -> String
drawSeatLayout sl@SeatLayout{..} = unlines [mkLine r | r <- [0..pred height]]
  where
    mkLine r = [cellVal sl (Cell r c) | c <- [0..pred width]]

solve' :: Int -> (Cell -> SeatLayout -> [Cell]) -> SeatLayout -> Int
solve' threshold neighbors = go
  where
    go sl =
      let sl' = step threshold neighbors sl
      in if sl' == sl
         then length $ filter (== '#') (Map.elems $ cells sl')
         else go sl'

part1 :: SeatLayout -> Int
part1 = solve' 4 immediateNeighbors

part2 :: SeatLayout -> Int
part2 = solve' 5 extensiveNeighbors

test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 37 (part1 example)
         , mkTestCase "part1 input" 2361 (part1 input')
         , mkTestCase "extensiveNeighbors 1" 8
                      (length $ extensiveNeighbors (Cell 4 3) part2Example1)
         , mkTestCase "extensiveNeighbors 2" 1
                      (length $ extensiveNeighbors (Cell 1 1) part2Example2)
         , mkTestCase "extensiveNeighbors 3" 0
                      (length $ extensiveNeighbors (Cell 3 3) part2Example3)
         , mkTestCase "part2 example" 26 (part2 example)
         , mkTestCase "part2 input" 2119 (part2 input')
         ]
-- test
solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
