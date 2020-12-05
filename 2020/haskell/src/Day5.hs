module Day5 (test, solve) where

import AoCUtils
import Data.List

input :: IO [Int]
input =
  map (\spec -> let (row, col) = findRowCol spec in row * 8 + col)
  . lines
  <$> readFile "input/day5.txt"

findSpec :: Char -> Char -> Int -> String -> Int
findSpec loChar hiChar maxVal spec = go spec 0 maxVal
  where
    go [] lo _ = lo
    go (x:xs) lo hi
      | x == loChar = go xs lo ((hi + lo) `div` 2)
      | x == hiChar = go xs (((hi + lo) `div` 2) + 1) hi

findRow :: String -> Int
findRow = findSpec 'F' 'B' 127
--findRow "FBFBBFF"
findCol :: String -> Int
findCol = findSpec 'L' 'R' 7
--findCol "RLR"
findRowCol :: String -> (Int, Int)
findRowCol spec = (findRow rowSpec, findCol colSpec)
  where
    (rowSpec, colSpec) = splitAt 7 spec

part1 :: [Int] -> Int
part1 ids = maximum ids

part2 :: [Int] -> Int
part2 ids = (+1) . fst . head . dropWhile (\(x, y) -> x + 1 == y) $ zippedIds
  where
    sortedIds = sort ids
    zippedIds = zip sortedIds (tail sortedIds)

test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 915 (part1 input')
         , mkTestCase "part2 input" 699 (part2 input')
         ]
--test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
--solve
