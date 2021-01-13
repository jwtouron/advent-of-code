module Main where

import Data.Maybe
import System.Environment

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20

data Type = Test | Solve deriving (Eq, Show)

commands :: [((Type, Int), IO ())]
commands =
  [ ((Test, 1), Day1.test)
  , ((Solve, 1),  Day1.solve)
  , ((Test, 2), Day2.test)
  , ((Solve, 2), Day2.solve)
  , ((Test, 3), Day3.test)
  , ((Solve, 3), Day3.solve)
  , ((Test, 4), Day4.test)
  , ((Solve, 4), Day4.solve)
  , ((Test, 5), Day5.test)
  , ((Solve, 5), Day5.solve)
  , ((Test, 6), Day6.test)
  , ((Solve, 6), Day6.solve)
  , ((Test, 7), Day7.test)
  , ((Solve, 7), Day7.solve)
  , ((Test, 8), Day8.test)
  , ((Solve, 8), Day8.solve)
  , ((Test, 9), Day9.test)
  , ((Solve, 9), Day9.solve)
  , ((Test, 10), Day10.test)
  , ((Solve, 10), Day10.solve)
  , ((Test, 11), Day11.test)
  , ((Solve, 11), Day11.solve)
  , ((Test, 12), Day12.test)
  , ((Solve, 12), Day12.solve)
  , ((Test, 13), Day13.test)
  , ((Solve, 13), Day13.solve)
  , ((Test, 14), Day14.test)
  , ((Solve, 14), Day14.solve)
  , ((Test, 15), Day15.test)
  , ((Solve, 15), Day15.solve)
  , ((Test, 16), Day16.test)
  , ((Solve, 16), Day16.solve)
  , ((Test, 17), Day17.test)
  , ((Solve, 17), Day17.solve)
  , ((Test, 18), Day18.test)
  , ((Solve, 18), Day18.solve)
  , ((Test, 19), Day19.test)
  , ((Solve, 19), Day19.solve)
  , ((Test, 20), Day20.test)
  , ((Solve, 20), Day20.solve)
  ]

main :: IO ()
main = do
  args <- getArgs
  let args' = if head args == "-t"
              then (Test, read (args !! 1))
              else (Solve, read (head args))
  fromJust $ lookup args' commands
