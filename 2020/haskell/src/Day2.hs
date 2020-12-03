module Day2 (test, solve) where

import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Monoid (Sum(..))
import AoCUtils

data Policy =
  Policy { num1 :: Int
         , num2 :: Int
         , char :: Char
         } deriving (Show)

data Line =
  Line { policy :: Policy
       , password :: String
       } deriving (Show)

parseLine :: String -> Line
parseLine line =
  Line (Policy (read num1) (read num2) (head char))
       password
 where
  [policy, password] = splitOn ": " line
  [nums, char] = splitOn " " policy
  [num1, num2] = splitOn "-" nums

input :: IO [Line]
input = map parseLine . lines <$> readFile "input/day2.txt"

monoidWhen :: Monoid m => Bool -> m -> m
monoidWhen b m = if b then m else mempty

part1 :: [Line] -> Int
part1 lines = getSum $ foldMap f lines
 where
  f (Line policy password) =
    monoidWhen (charCount >= num1 policy && charCount <= num2 policy) (Sum 1)
   where
    charCount = getSum $ foldMap g password
    g c = monoidWhen (c == char policy) (Sum 1)

part2 :: [Line] -> Int
part2 lines = getSum $ foldMap f lines
 where
  f (Line policy password) =
    monoidWhen (elem matches [[True, False], [False, True]]) (Sum 1)
   where
    matches = [ password !! (num1 policy - 1) == char policy
              , password !! (num2 policy - 1) == char policy]

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 460 (part1 input')
         , mkTestCase "part2 input" 251 (part2 input')
         ]
-- test
solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
