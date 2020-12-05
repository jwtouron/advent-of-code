module Main where

import Data.Maybe
import System.Environment

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

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
  ]

main :: IO ()
main = do
  args <- getArgs
  let args' = if head args == "-t"
              then (Test, (read $ args !! 1))
              else (Solve, (read $ head args))
  fromJust $ lookup args' commands
