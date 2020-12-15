{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day15 (test, solve) where

import AoCUtils
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List

data Seen =
  SeenOnce Int | SeenTwice (Int, Int) deriving (Show)

type SeenMap = IntMap Seen

data Game =
  Game { turn :: !Int
       , lastSpoken :: !Int
       , seenMap :: !SeenMap
       } deriving Show

gameFromList :: [Int] -> Game
gameFromList xs = Game (succ $ length xs) (last xs) seenMap
  where
    seenMap = foldl' f IntMap.empty $ zip [1..] xs
      where
        f m (t, x) = IntMap.insert x (SeenOnce t) m

input :: [Int]
input = [15, 5, 1, 4, 7, 0]

takeTurn :: Game -> Game
takeTurn Game{..} =
  Game { turn = turn', lastSpoken = lastSpoken', seenMap = seenMap' }
  where
    turn' = succ turn
    lastSpoken' =
      case seenMap IntMap.! lastSpoken of
        SeenOnce _ -> 0
        SeenTwice (t1, t2) -> t1 - t2
    seenMap' = IntMap.alter f lastSpoken' seenMap
      where
        f = \case
          Nothing -> Just $ SeenOnce turn
          Just (SeenOnce t) -> Just $ SeenTwice (turn, t)
          Just (SeenTwice (t1, _)) -> Just $ SeenTwice (turn, t1)

solve' :: Int -> [Int] -> Int
solve' turnNum =
  lastSpoken . head . dropWhile ((< succ turnNum) . turn) . iterate takeTurn . gameFromList

part1 :: [Int] -> Int
part1 = solve' 2020

part2 :: [Int] -> Int
part2 = solve' 30000000

test :: IO ()
test = do
  mkTest [ mkTestCase "part1 input" 1259 (part1 input)
         -- , mkTestCase "part2 input" 689 (part2 input)
         ]

solve :: IO ()
solve = do
  print (part1 input)
  print (part2 input)
