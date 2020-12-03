module Day2 (test, solve) where

import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Monoid (Sum(..))

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
  part1' <- fmap part1 input
  part2' <- fmap part2 input
  when (part1' /= 460)
       (error "part1 is not 460")
  when (part2' /= 251)
       (error "part2 is not 251")

solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
