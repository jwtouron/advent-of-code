{-# LANGUAGE BangPatterns #-}

module Day25 (test, solve) where

import AoCUtils

input :: (Int, Int)
input = (15733400, 6408062)

findLoopSize :: Int -> Int -> Int
findLoopSize subjectNum limit = go 0 1
  where
    go !ls x =
      if x == limit
      then ls
      else go (ls + 1) (x * subjectNum `rem` 20201227)

transform :: Int -> Int -> Int
transform subjectNum loopSize = go loopSize 1
  where
    go 0 !x = x
    go ls !x = go (ls - 1) (x * subjectNum `rem` 20201227)

part1 :: (Int, Int) -> Int
part1 (key1, key2) =
  let loopSize = findLoopSize 7 key1
  in transform key2 loopSize

test :: IO ()
test = do
  mkTest [ mkTestCase "findLoopSize 7 5764801" 8 $ findLoopSize 7 5764801
         , mkTestCase "findLoopSize 7 17807724" 11 $ findLoopSize 7 17807724
         , mkTestCase "part1 example" 14897079 $ part1 (5764801, 17807724)
         , mkTestCase "part1 input" 16457981 $ part1 input
         ]

solve :: IO ()
solve = do
  print $ part1 input
