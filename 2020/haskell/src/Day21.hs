module Day21 (test, solve) where

import AoCUtils
import Data.Foldable (toList)
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Food =
  Food { ingredients :: Set String
       , allergens :: Set String
       } deriving (Show)

parseFood :: String -> Food
parseFood line =
  let [is, as] = splitOn " (contains " line
  in Food (Set.fromList $ splitOn " " is) (Set.fromList $ splitOn ", " (init as))

example :: [Food]
example = map parseFood ls
  where
    ls = [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
         , "trh fvjkl sbzzf mxmxvkd (contains dairy)"
         , "sqjhc fvjkl (contains soy)"
         , "sqjhc mxmxvkd sbzzf (contains fish)"
         ]

input :: IO [Food]
input = map parseFood . lines <$> readFile "input/day21.txt"

type Analysis = Map String (Set String)

analyze :: [Food] -> Analysis
analyze foods = go initialAnalysis
  where
    go analysis =
      let analysis' = foldr f analysis singles
      in if analysis' == analysis
         then analysis
         else go analysis'
      where
        f ing analysis' = Map.map g analysis'
          where
            g as = if Set.size as > 1 then Set.delete ing as else as
        singles = map (head . toList) $ filter ((== 1) . Set.size) $ Map.elems analysis
    initialAnalysis = foldr f Map.empty foods
      where
        f (Food is as) m = foldr g m as
          where
            g a m =
              case Map.lookup a m of
                Nothing -> Map.insert a is m
                Just is' -> Map.insert a (Set.intersection is' is) m

part1 :: [Food] -> Analysis -> Int
part1 foods analysis = sum [1 | (Food is _) <- foods, i <- toList is, Set.member i definitelyGoods]
  where
    possiblyBads = Set.unions $ Map.elems analysis
    allIngredients = Set.unions (map ingredients foods)
    definitelyGoods = Set.difference allIngredients possiblyBads

part2 :: Analysis -> String
part2 = intercalate "," . map (head . toList . snd) . sortOn fst . Map.assocs

test :: IO ()
test = do
  input' <- input
  let analysis = analyze input'
  mkTest [ mkTestCase "part1 example" 5 (part1 example $ analyze example)
         , mkTestCase "part1 input" 2614 (part1 input' analysis)
         , mkTestCase "part2 example" "mxmxvkd,sqjhc,fvjkl" (part2 $ analyze example)
         , mkTestCase "part2 input" "qhvz,kbcpn,fzsl,mjzrj,bmj,mksmf,gptv,kgkrhg" (part2 analysis)
         ]

solve :: IO ()
solve = do
  input' <- input
  let analysis = analyze input'
  print (part1 input' analysis)
  print (part2 analysis)
