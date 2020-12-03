module Main where

import System.Environment

import qualified Day1
import qualified Day2

main :: IO ()
main = do
  args <- getArgs
  let (doTest, day) = if head args == "-t"
                      then (True, read $ args !! 1)
                      else (False, read $ head args)

  case (doTest, day) of
    (True, 1) -> Day1.test
    (False, 1) -> Day1.solve
    (True, 2) -> Day2.test
    (False, 2) -> Day2.solve
    _ -> error "Unknown day"
