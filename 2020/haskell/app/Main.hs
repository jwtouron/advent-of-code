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
  ]

main :: IO ()
main = do
  args <- getArgs
  let args' = if head args == "-t"
              then (Test, (read $ args !! 1))
              else (Solve, (read $ head args))
  fromJust $ lookup args' commands
