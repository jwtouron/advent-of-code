{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Day10 (test, solve) where

import AoCUtils
import Control.Monad.State.Strict
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

smallExample :: [Int]
smallExample =
  [ 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 ]

bigExample :: [Int]
bigExample =
  [ 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38
  , 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3 ]

input :: IO [Int]
input = map read . lines <$> readFile "input/day10.txt"

part1 :: [Int] -> Int
part1 xs =
  let xs' = sort xs
      (numOnes, numThrees) = go 0 xs' (0, 0)
  in numOnes * succ numThrees
  where
    go _ [] nums = nums
    go prev (x:xs) (!numOnes, !numThrees) =
      case x - prev of
        1 -> go x xs (succ numOnes, numThrees)
        2 -> go x xs (numOnes, numThrees)
        3 -> go x xs (numOnes, succ numThrees)
        _ -> error "Something went wrong"

part2 :: [Int] -> Int
part2 = flip evalState IntMap.empty . go 0 0 . sort
  where
    go :: Int -> Int -> [Int] -> State (IntMap Int) Int
    go _ _ [] = pure 1
    go _ _ (_:[]) = pure 1
    go _ curr (_:y:[])
      | y - curr <= 3 = pure 2
      | otherwise = pure 1
    go ix curr (x:y:z:[])
      | z - curr <= 3 = pure 4
      | y - curr <= 3 = pure 2
      | otherwise = lookupOrCalc ix [go (succ ix) x (y:z:[])]
    go ix curr (x:y:z:xs)
      | z - curr <= 3 = do
          let calcs = [ go (ix + 1) x (y:z:xs)
                      , go (ix + 2) y (z:xs)
                      , go (ix + 3) z xs
                      ]
          lookupOrCalc ix calcs
      | y - curr <= 3 = do
          let calcs = [ go (ix + 2) y (z:xs)
                      , go (ix + 3) z xs
                      ]
          lookupOrCalc ix calcs
      | otherwise = lookupOrCalc ix [go (ix + 1) x (y:z:xs)]

    lookupOrCalc :: Int -> [State (IntMap Int) Int] -> State (IntMap Int) Int
    lookupOrCalc ix calcs = do
      cache <- get
      case IntMap.lookup ix cache of
        Just x -> pure x
        Nothing -> do
          x <- sum <$> sequence calcs
          modify' (IntMap.insert ix x)
          pure x

test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 2170 (part1 input')
         , mkTestCase "part2 [1]" 1 (part2 [1])
         , mkTestCase "part2 [1,3]" 2 (part2 [1,3])
         , mkTestCase "part2 [3,6]" 1 (part2 [3,6])
         , mkTestCase "part2 [1,2,3]" 4 (part2 [1,2,3])
         , mkTestCase "part2 smallExample" 8 (part2 smallExample)
         , mkTestCase "part2 bigExample" 19208 (part2 bigExample)
         , mkTestCase "part2 input" 24803586664192 (part2 input')
         ]
--test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
