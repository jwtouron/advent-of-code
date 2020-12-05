module Day4 (test, solve) where

import Control.Monad
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Text.Regex.TDFA (Regex)
import qualified Text.Regex.TDFA as RE
import AoCUtils

mapsFromLines :: [String] -> [Map String String]
mapsFromLines = map mapFromPassport . breakPassports
  where
    breakPassports :: [String] -> [[String]]
    breakPassports = go [] []
      where
        go accum _ [] = accum
        go accum curr ("":ls) = go (accum ++ [curr]) [] ls
        go accum curr (l:ls) = go accum (curr ++ [l]) ls
    mapFromPassport :: [String] -> Map String String
    mapFromPassport passport =
      foldl' (\m (k:v:[]) -> Map.insert k v m) Map.empty
      $ concatMap (map (splitOn ":") . words) passport

invalids :: [Map String String]
invalids = mapsFromLines lines
  where
    lines = [ "eyr:1972 cid:100"
            , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
            , ""
            , "iyr:2019"
            , "hcl:#602927 eyr:1967 hgt:170cm"
            , "ecl:grn pid:012533040 byr:1946"
            , ""
            , "hcl:dab227 iyr:2012"
            , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
            , ""
            , "hgt:59cm ecl:zzz"
            , "eyr:2038 hcl:74454a iyr:2023"
            , "pid:3556412378 byr:2007"
            ]

input :: IO [Map String String]
input = mapsFromLines . lines <$> readFile "input/day4.txt"

isValidPart1 :: Map String String -> Bool
isValidPart1 m =
  Map.size m == 8 || (Map.size m == 7 && Map.notMember "cid" m)

yearRegex, hgtRegex, hclRegex, eclRegex, pidRegex :: Regex
yearRegex = RE.makeRegex "^[[:digit:]]{4}$"
hgtRegex = RE.makeRegex "^[[:digit:]]+"
hclRegex = RE.makeRegex "^#[0-9a-f]{6}$"
eclRegex = RE.makeRegex "^(amb|blu|brn|gry|grn|hzl|oth)$"
pidRegex = RE.makeRegex "^[[:digit:]]{9}$"

numInRange :: (Num a, Ord a) => a -> a -> a -> Bool
numInRange lo hi year = year >= lo && year <= hi

checkByr, checkIyr, checkEyr, checkHgt, checkHcl, checkEcl, checkPid :: String -> Maybe ()

checkByr byr = do
  guard $ RE.match yearRegex byr
  guard $ numInRange 1920 2002 (read byr :: Int)

checkIyr iyr = do
  guard $ RE.match yearRegex iyr
  guard $ numInRange 2010 2020 (read iyr :: Int)

checkEyr eyr = do
  guard $ RE.match yearRegex eyr
  guard $ numInRange 2020 2030 (read eyr :: Int)

checkHgt hgt = do
  let match =  RE.match hgtRegex hgt :: (String, String, String)
  case match of
    ("", ds, "in") -> guard $ numInRange 59 76 (read ds :: Int)
    ("", ds, "cm") -> guard $ numInRange 150 193 (read ds :: Int)
    _ -> fail $ "Bad hgt: " ++ hgt

checkHcl = guard . RE.match hclRegex

checkEcl = guard . RE.match eclRegex
checkPid = guard . RE.match pidRegex

isValidPart2 :: Map String String -> Bool
isValidPart2 m = all isJust $ map join
  [ checkByr <$> Map.lookup "byr" m
  , checkIyr <$> Map.lookup "iyr" m
  , checkEyr <$> Map.lookup "eyr" m
  , checkHgt <$> Map.lookup "hgt" m
  , checkHcl <$> Map.lookup "hcl" m
  , checkEcl <$> Map.lookup "ecl" m
  , checkPid <$> Map.lookup "pid" m
  ]

solve' :: (Map String String -> Bool) -> [Map String String] -> Int
solve' isValid = length . filter isValid

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 202 (solve' isValidPart1 input')
         , mkTestCase "part2 input" 137 (solve' isValidPart2 input')
         ]
--test
solve :: IO ()
solve = do
  input' <- input
  print (solve' isValidPart1 input')
  print (solve' isValidPart2 input')
