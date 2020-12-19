module Day13 (test, solve) where

import AoCUtils
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Input = (Int, [String])

example :: Input
example = (939, splitOn "," "7,13,x,x,59,x,31,19")

input :: IO Input
input = do
  e:ids:_ <- lines <$> readFile "input/day13.txt"
  pure ((read e), splitOn "," ids)

parseId :: String -> Maybe Int
parseId ('x':_) = Nothing
parseId x = Just (read x)

part1 :: Input -> Int
part1 input = uncurry (*) $ fst &&& snd $ head validBusIds
  where
    timeline :: [(Int, Int)]
    timeline = iterate (bimap succ succ) (0 :: Int, earliest)
    validBusIds :: [(Int, Int)]
    validBusIds = mapMaybe findBusId timeline
    findBusId :: (Int, Int) -> Maybe (Int, Int)
    findBusId (offset, earliest) =
      let ids' = mapMaybe (\id' -> if rem earliest id' == 0 then Just id' else Nothing) ids
      in if length ids' == 0 then Nothing else Just (offset, head ids')
    (earliest, ids) = (fst input, mapMaybe parseId $ snd input)


crt :: (Show a, Integral a) => [(a, a)] -> a
crt terms = (sum $ map f terms) `mod` termsProd
  where
    termsProd = product $ map snd terms
    f (a, n) = a * modInv (termsProd `div` n) n * (termsProd `div` n)
    modInv a m
      | 1 == g = mkPos i
      | otherwise = error $ "Cannot modInv: " ++ show (a, m)
      where
        (i, _, g) = gcdExt a m
        mkPos x
          | x < 0 = x + m
          | otherwise = x
    gcdExt a 0 = (1, 0, a)
    gcdExt a b =
      let (q, r) = a `quotRem` b
          (s, t, g) = gcdExt b r
      in (t, s - q * t, g)

part2 :: Input -> Int
part2 input =
  crt
  $ foldr (\(a, n) ts -> if "x" == n then ts else (negate a, read n):ts) []
  $ zip [0..] $ snd input
-- input >>= pure. part2
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 295 (part1 example)
         , mkTestCase "part1 input" 153 (part1 input')
         , mkTestCase "part2 input" 471793476184394 (part2 input')
         ]
--test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
