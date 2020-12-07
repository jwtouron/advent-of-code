module Day7 (test, solve) where

import AoCUtils
import Control.Applicative
import Data.Char (isDigit)
import Data.Graph (graphFromEdges, transposeG)
import qualified Data.Graph as Graph
import Data.Maybe (fromJust)
import qualified Data.HashSet as Set
import qualified Text.Regex.Applicative as RE

type RE = RE.RE

type Set = Set.HashSet

data Rule =
  Rule { bag :: String
       , contains :: Set (Int, String)
       } deriving Show

parseInt :: RE Char Int
parseInt = read <$> some (RE.psym isDigit)

parseContainingBag :: RE Char String
parseContainingBag =
  some RE.anySym <* RE.sym ' ' <* RE.string "bags contain" <* some (RE.sym ' ')

parseNoOtherBags :: RE Char (Set (Int, String))
parseNoOtherBags = RE.string "no other bags" *> pure (Set.empty)

parseContainedBag :: RE Char (Int, String)
parseContainedBag =
  (,) <$> parseInt <* some (RE.sym ' ') <*> RE.few RE.anySym
  <* RE.sym ' ' <* RE.string "bag" <* RE.few (RE.sym 's')

parseSomeContainedBags :: RE Char (Set (Int, String))
parseSomeContainedBags =
  (\b bs -> Set.fromList (b:bs))
  <$> parseContainedBag
  <*> many (RE.string ", " *> parseContainedBag)

parseContainedBags :: RE Char (Set (Int, String))
parseContainedBags =
  (parseNoOtherBags <|> parseSomeContainedBags) <* RE.sym '.'

parseRule :: RE Char Rule
parseRule =
  Rule <$> parseContainingBag <*> parseContainedBags

ruleMatcher :: String -> Maybe Rule
ruleMatcher = (RE.=~ parseRule)

type Input =
  ( Graph.Graph
  , Graph.Vertex -> (Rule, String, [String])
  , String -> Maybe Graph.Vertex)

graphFromLines :: [String] -> Input
graphFromLines = graphFromEdges . map (f . fromJust . ruleMatcher)
  where
    f rule@(Rule bag contains) =
      (rule, bag, map snd . Set.toList $ contains)

example :: Input
example = graphFromLines ls
  where
    ls = [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
         , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
         , "bright white bags contain 1 shiny gold bag."
         , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
         , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
         , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
         , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
         , "faded blue bags contain no other bags."
         , "dotted black bags contain no other bags."
         ]

input :: IO Input
input = graphFromLines . lines <$> readFile "input/day7.txt"

part1 :: Input -> Int
part1 (graph, _, vertexFromKey) =
  (subtract 1)
  . length
  . flip Graph.reachable (fromJust $ vertexFromKey "shiny gold")
  . transposeG
  $ graph

part2 :: Input -> Int
part2 (_, nodeFromVertex, vertexFromKey) =
  1 `subtract` (go $ nodeFromKey "shiny gold")
  where
    go ((Rule _ contains), _, _) =
      1 + (sum $ map f $ Set.toList contains)
      where
        f (x, key) = x * go (nodeFromKey key)
    nodeFromKey = nodeFromVertex . fromJust . vertexFromKey
-- input >>= pure . part2
-- part2 example
test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 222 (part1 input')
         , mkTestCase "part2 input" 13264 (part2 input')
         , mkTestCase "part2 example" 32 (part2 example)
         ]
-- test
solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
