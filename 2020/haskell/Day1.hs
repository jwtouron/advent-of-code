module Day1 (solve, test) where

input :: IO [Int]
input = map read . lines <$> readFile "input/day1.txt"

part1 :: [Int] -> Int
part1 input = head [x * y | x <- input, y <- input, x + y == 2020]
-- fmap part1 input
-- => 41979

part2 :: [Int] -> Int
part2 input = head [x * y * z| x <- input, y <- input, z <- input, x + y + z == 2020]
-- fmap part2 input
-- => 193416912

test :: IO ()
test = undefined

solve :: IO ()
solve = do
  input' <- input
  print $ part1 input'
  print $ part2 input'
