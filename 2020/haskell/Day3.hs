module Day3 (test, solve) where

import Control.Monad (when)
import Data.Array (Array)
import qualified Data.Array as Array
import AoCUtils

parseLines :: [String] -> Array (Int, Int) Char
parseLines inputLines = Array.array bounds elems
 where
  numCols = length $ inputLines !! 0
  numRows = length inputLines
  bounds = ((0, 0), (numRows - 1, numCols - 1))
  elems = [((row, col), ch) | (row, line) <- zip [0..] inputLines
                            , (col, ch) <- zip [0..] line]

example :: Array (Int, Int) Char
example = parseLines lines
 where
  lines =
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]

input :: IO (Array (Int, Int) Char)
input = parseLines . lines <$> readFile "input/day3.txt"

countTrees :: Array (Int, Int) Char -> Int -> Int -> Int
countTrees arr right down = go (0, 0) 0
 where
  go (r, c) res =
    let pos'@(r', c') = (r + down, ((c + right) `rem` (maxCol + 1)))
        res' = if arr Array.! (r', c') == '#'
               then res + 1 else res
    in
      if r' > maxRow
      then res
      else go pos' res'
  (_, (maxRow, maxCol)) = Array.bounds arr

part1 :: Array (Int, Int) Char -> Int
part1 arr = countTrees arr 3 1

part2 :: Array (Int, Int) Char -> Int
part2 arr = product [c1, c2, c3, c4, c5]
 where
  c1 = countTrees arr 1 1
  c2 = countTrees arr 3 1
  c3 = countTrees arr 5 1
  c4 = countTrees arr 7 1
  c5 = countTrees arr 1 2

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 7 (part1 example)
         , mkTestCase "part1 input" 184 (part1 input')
         , mkTestCase "part2 example" 336 (part2 example)
         , mkTestCase "part2 input" 2431272960 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  print $ part1 input'
  print $ part2 input'
