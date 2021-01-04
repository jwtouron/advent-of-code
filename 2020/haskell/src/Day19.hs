module Day19 (test, solve) where

import AoCUtils
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP as ReadP

type Parser = ReadP

toTuple2 :: [a] -> (a, a)
toTuple2 [x, y] = (x, y)
toTuple2 _ = error "toTuple2: Wrong number of list items."

data Rule =
    Lit Int Char
  | Sub Int [[Int]]
  deriving (Eq, Show)

ruleNum :: Rule -> Int
ruleNum (Lit n _) = n
ruleNum (Sub n _) = n

parseRule :: String -> Rule
parseRule = fst . head . readP_to_S (parser <* eof)
  where
    parser :: Parser Rule
    parser = do
      rn <- read <$> many1 (satisfy isDigit) <* string ": "
      litP rn <|> subP rn
    litP n = Lit n <$> (char '"' *> satisfy (const True) <* char '"')
    subP :: Int -> Parser Rule
    subP n = (Sub n .) . (:) <$> seqP <*> ReadP.many (string "| " *> seqP)
    seqP :: Parser [Int]
    seqP = many1 (read <$> munch1 isDigit <* ReadP.many (char ' '))

type RuleMap = IntMap Rule
type Input = (RuleMap, [String])

parseInput :: String -> Input
parseInput =
  first (IntMap.fromList . map ((ruleNum &&& id) . parseRule))
  . toTuple2 . splitOn [""] . lines

example1 :: Input
example1 = parseInput "0: 4 1 5\n\
\1: 2 3 | 3 2\n\
\2: 4 4 | 5 5\n\
\3: 4 5 | 5 4\n\
\4: \"a\"\n\
\5: \"b\"\n\
\\n\
\ababbb\n\
\bababa\n\
\abbbab\n\
\aaabbb\n\
\aaaabbb"

example2 :: Input
example2 = parseInput "42: 9 14 | 10 1\n\
\9: 14 27 | 1 26\n\
\10: 23 14 | 28 1\n\
\1: \"a\"\n\
\11: 42 31\n\
\5: 1 14 | 15 1\n\
\19: 14 1 | 14 14\n\
\12: 24 14 | 19 1\n\
\16: 15 1 | 14 14\n\
\31: 14 17 | 1 13\n\
\6: 14 14 | 1 14\n\
\2: 1 24 | 14 4\n\
\0: 8 11\n\
\13: 14 3 | 1 12\n\
\15: 1 | 14\n\
\17: 14 2 | 1 7\n\
\23: 25 1 | 22 14\n\
\28: 16 1\n\
\4: 1 1\n\
\20: 14 14 | 1 15\n\
\3: 5 14 | 16 1\n\
\27: 1 6 | 14 18\n\
\14: \"b\"\n\
\21: 14 1 | 1 14\n\
\25: 1 1 | 1 14\n\
\22: 14 14\n\
\8: 42\n\
\26: 14 22 | 1 20\n\
\18: 15 15\n\
\7: 14 5 | 1 21\n\
\24: 14 1\n\
\\n\
\abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
\bbabbbbaabaabba\n\
\babbbbaabbbbbabbbbbbaabaaabaaa\n\
\aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
\bbbbbbbaaaabbbbaaabbabaaa\n\
\bbbababbbbaaaaaaaabbababaaababaabab\n\
\ababaaaaaabaaab\n\
\ababaaaaabbbaba\n\
\baabbaaaabbaaaababbaababb\n\
\abbbbabbbbaaaababbbbbbaaaababb\n\
\aaaaabbaabaaaaababaa\n\
\aaaabbaaaabbaaa\n\
\aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
\babaaabbbaaabaababbaabababaaab\n\
\aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

input :: IO Input
input = parseInput <$> readFile "input/day19.txt"

parserFromRuleMap :: RuleMap -> Parser ()
parserFromRuleMap ruleMap = go (lookup 0) <* eof
  where
    go :: Rule -> Parser ()
    go (Lit _ c) = void (char c)
    go (Sub _ xss) = choice (map (mapM_ (go . lookup)) xss)
    lookup = (ruleMap IntMap.!)

part2RuleMapUpdate :: RuleMap -> RuleMap
part2RuleMapUpdate =
  IntMap.insert 8 (Sub 8 [[42], [42, 8]])
  . IntMap.insert 11 (Sub 11 [[42, 31], [42, 11, 31]])

solve' :: Input -> Int
solve' (ruleMap, messages) = length $ concatMap (readP_to_S parser) messages
  where parser = parserFromRuleMap ruleMap

part1 :: Input -> Int
part1 = solve'

part2 :: Input -> Int
part2 = solve' . first part2RuleMapUpdate

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example1" 2 (part1 example1)
         , mkTestCase "part1 example2" 3 (part1 example2)
         , mkTestCase "part1 input" 149 (part1 input')
         , mkTestCase "part2 example2" 12 (part2 example2)
         , mkTestCase "part2 input" 332 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  pure ()
  print (part1 input')
  print (part2 input')
