{-# LANGUAGE GADTs, NumericUnderscores, StandaloneDeriving, TypeApplications #-}

module Day23 (test, solve) where

import AoCUtils
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as Vec
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable as MVec
import Data.Vector.Unboxed.Mutable (MVector)

example :: [Int]
example = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [7, 1, 6, 8, 9, 2, 5, 4, 3]

data State where
  State :: { current :: Int, cups :: Vector Int, maxLabel :: Int } -> State

deriving instance Show State

stateCupsToList :: State -> [Int]
stateCupsToList (State _ cups mxLbl) = go 1 (Vec.length cups - 1) []
  where
    go _ 0 acc = reverse acc
    go i n acc = let i' = cups Vec.! i in go i' (n - 1) (i' : acc)

newStatePart1 :: [Int] -> State
newStatePart1 xs = State (head xs) cups (maximum xs)
  where
    cups = Vec.create $ do
      v <- MVec.new (1 + length xs)
      mapM_ (uncurry (MVec.unsafeWrite v)) (zip xs (tail xs ++ [head xs]))
      return v

newStatePart2 :: [Int] -> State
newStatePart2 xs = State (head xs) cups mx
  where
    mx = 1_000_000
    xsLength = length xs
    cups = Vec.create $ do
      v <- MVec.new (mx + 1)
      mapM_ (uncurry (MVec.unsafeWrite v)) (zip xs' (tail xs' ++ [head xs']))
      return v
    xs' = xs ++ [maximum xs + 1 .. mx]

selectDestination :: Int -> Int -> (Int, Int, Int) -> Int
selectDestination mxLbl 0 cups = selectDestination mxLbl mxLbl cups
selectDestination mxLbl curr cups@(a, b, c)
  | curr == a || curr == b || curr == c = selectDestination mxLbl (curr - 1) cups
selectDestination _ curr _ = curr

step :: State -> State
step (State curr cups mxLbl) = State curr' cups' mxLbl
  where
    curr' = cups' Vec.! curr
    cups' = runST $ do
      v <- Vec.unsafeThaw @Int @(ST _) cups
      MVec.unsafeWrite v curr =<< MVec.unsafeRead v c
      MVec.unsafeWrite v c =<< MVec.unsafeRead v dest
      MVec.unsafeWrite v dest a
      Vec.unsafeFreeze v
    a = cups Vec.! curr
    b = cups Vec.! a
    c = cups Vec.! b
    dest = selectDestination mxLbl (curr - 1) (a, b, c)

part1 :: [Int] -> String
part1 xs =
  map (head . show)
  . take (maxLabel state - 1)
  . stateCupsToList
  . (!! 100)
  . iterate step $ state
  where
    state = newStatePart1 xs

part2 :: [Int] -> Int
part2 xs = a * b
  where
    state = (!! 10_000_000) $ iterate step $ newStatePart2 xs
    a = cups state Vec.! 1
    b = cups state Vec.! a

test :: IO ()
test = do
  mkTest [ mkTestCase "part1 example" "67384529" (part1 example)
         , mkTestCase "part1 input" "49725386" (part1 input)
         , mkTestCase "part2 example" 149245887792 (part2 example)
         , mkTestCase "part2 input" 538935646702 (part2 input)
         ]

solve :: IO ()
solve = do
  print $ part1 input
  print $ part2 input
