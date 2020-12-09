module Day9 (test, solve) where

import AoCUtils
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

example :: Vector Int
example = Vec.fromList $ map read nums
  where
    nums = [ "35", "20", "15", "25", "47", "40", "62", "55", "65", "95"
           , "102", "117", "150", "182", "127", "219", "299", "277", "309", "576"
           ]

input :: IO (Vector Int)
input = Vec.fromList . map read . lines <$> readFile "input/day9.txt"

part1 :: Int -> Vector Int -> Int
part1 preamble vec = go preamble
  where
    go n
      | n == Vec.length vec = error "Reached end, didn't find"
      | otherwise =
          if hasSummingNums (n - preamble) IntSet.empty
          then go (n + 1)
          else goal
      where
        goal = vec Vec.! n
        hasSummingNums i seen
          | i == n = False
          | otherwise =
              let ival = vec Vec.! i
              in
                if IntSet.member (goal - ival) seen && ival + ival /= goal
                then True
                else hasSummingNums (i + 1) (IntSet.insert ival seen)

part2 :: Int -> Vector Int -> Int
part2 preamble vec = go 0
  where
    invalidNum = part1 preamble vec
    atEnd = (== invalidNum) . (vec Vec.!)
    go i =
      case go' (succ i) (vec Vec.! i) (vec Vec.! i) (vec Vec.! i) of
        Just (lo, hi) -> lo + hi
        Nothing -> go (succ i)
      where
        go' j sum lowest highest
          | atEnd j = Nothing
          | sum > invalidNum = Nothing
          | sum == invalidNum = Just (lowest, highest)
          | otherwise =
              let jval = vec Vec.! j
                  lowest' = if jval < lowest then jval else lowest
                  highest' = if jval > highest then jval else highest
              in go' (succ j) (sum + jval) lowest' highest'

test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 127 (part1 5 example)
         , mkTestCase "part1 input" 57195069 (part1 25 input')
         , mkTestCase "part2 example" 62 (part2 5 example)
         , mkTestCase "part2 input" 7409241 (part2 25 input')
         ]

solve = do
  input' <- input
  print (part1 25 input')
  print (part2 25 input')
