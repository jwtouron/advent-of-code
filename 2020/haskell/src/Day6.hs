module Day6 (test, solve) where

import AoCUtils
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

setsFromInput :: [String] -> [[Set Char]]
setsFromInput = map setsFromGroup . breakGroups
  where
    breakGroups :: [String] -> [[String]]
    breakGroups = go [] []
      where
        go :: [String] -> [[String]] -> [String] -> [[String]]
        go group groups [] = groups ++ [group]
        go group groups ("":ls) = go [] (groups ++ [group]) ls
        go group groups (l:ls) = go (group ++ [l]) groups ls
    setsFromGroup :: [String] -> [Set Char]
    setsFromGroup = foldr f []
      where
        f l ls = Set.fromList l : ls

example :: [[Set Char]]
example = setsFromInput input
  where
    input = [ "abc"
            , ""
            , "a"
            , "b"
            , "c"
            , ""
            , "ab"
            , "ac"
            , ""
            , "a"
            , "a"
            , "a"
            , "a"
            , ""
            , "b"
            ]

input :: IO [[Set Char]]
input = setsFromInput . lines <$> readFile "input/day6.txt"
-- input

part1 :: [[Set Char]] -> Int
part1 = sum . map (Set.size . Set.unions)
-- input >>= pure . part1
part2 :: [[Set Char]] -> Int
part2 = sum . map (Set.size . foldl1' Set.intersection)
-- input >>= pure. part2
test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 6430 (part1 input')
         , mkTestCase "part1 input" 3125 (part2 input')
         ]
--test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
