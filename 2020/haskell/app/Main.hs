module Main where

import Data.Maybe
import System.Environment

import Day1
import Day2
import Day3

data Type = Test | Solve deriving (Eq, Show)

data Args =
  Args Type Int deriving (Eq, Show)

commands :: [(Args, IO ())]
commands =
  [ (Args Test 1, Day1.test)
  , (Args Solve 1,  Day1.solve)
  , (Args Test 2, Day2.test)
  , (Args Solve 2, Day2.solve)
  , (Args Test 3, Day3.test)
  , (Args Solve 3, Day3.solve)
  ]

main :: IO ()
main = do
  args <- getArgs
  let args' = if head args == "-t"
              then Args Test (read $ args !! 1)
              else Args Solve (read $ head args)
  fromJust $ lookup args' commands
