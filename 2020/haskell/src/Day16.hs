module Day16 (test, solve) where

import AoCUtils
import Data.List
import Data.List.Split (splitOn)
import Data.Function

data Range = Range Int Int deriving (Eq, Ord, Show)

data Rule =
  Rule { field :: String
       , ranges :: [Range]
       } deriving (Eq, Ord, Show)

type Ticket = [Int]

type Input = ([Rule], Ticket, [Ticket])

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseRange :: String -> Range
parseRange s =
  let [x1,x2] = splitOn "-" s
  in Range (read x1) (read x2)

parseRule :: String -> Rule
parseRule s =
  let (field':ranges':_) = splitOn ": " s
      ranges'' = splitOn " or " ranges'
  in Rule field' (map parseRange ranges'')

example1 :: Input
example1 = (rules, myTicket, nearbyTickets)
  where
    rules = [ Rule "class" [Range 1 3, Range 5 7]
            , Rule "row" [Range 6 11, Range 33 44]
            , Rule "seat" [Range 13 40, Range 45 50]
            ]
    myTicket = [7, 1, 14]
    nearbyTickets = [[7,3,47], [40,4,50], [55,2,20], [38,6,12]]

example2 :: Input
example2 = (rules, myTicket, nearbyTickets)
  where
    rules = [ Rule "class" [Range 0 1, Range 4 19]
            , Rule "row" [Range 0 5, Range 8 19]
            , Rule "seat" [Range 0 13, Range 16 19]
            ]
    myTicket = [11, 12, 13]
    nearbyTickets = [[3, 9, 18], [15, 1, 5], [5, 14, 9]]

input :: IO Input
input = do
    [rules, ticket, tickets] <- splitOn [""] . lines <$> readFile "input/day16.txt"
    pure (map parseRule rules, parseTicket (last ticket), map parseTicket (tail tickets))

valueInRange :: Int -> Range -> Bool
valueInRange v (Range lo hi) = v >= lo && v <= hi

valueSatisfiesRule :: Int -> Rule -> Bool
valueSatisfiesRule v (Rule _ rs) = any (valueInRange v) rs

ticketInvalidValues :: Ticket -> [Rule] -> [Int]
ticketInvalidValues ticket rules = foldr f [] ticket
  where
    f val vals =
      if any (valueSatisfiesRule val) rules
      then vals
      else val:vals

determineFieldOrder :: [Ticket] -> [Rule] -> [String]
determineFieldOrder tickets rules =
  fixup $ head $ go sortedFields rules
  where
    ticketFields = transpose tickets
    fixup rs = [field r | t1 <- ticketFields, (r, t2) <- rs, t1 == t2]
    sortedFields =
      sortBy (compare `on` (length . filterSatisfyingRules rules)) ticketFields
    go :: [[Int]] -> [Rule] -> [[(Rule,[Int])]]
    go [] _ = [[]]
    go _ [] = []
    go (f:fields) rules' = do
      r <- filterSatisfyingRules rules' f
      map ((r,f):) $ go fields (delete r rules')
    filterSatisfyingRules rules' field' =
      filter (\r -> all (flip valueSatisfiesRule r) field') rules'

part1 :: Input -> Int
part1 (rules, _, tickets) =
  foldl' (\s t -> s + sum (ticketInvalidValues t rules)) 0 tickets

part2 :: Input -> Int
part2 (rules, myTicket, nearbyTickets) =
  product . map snd . filter (isDepartureField . fst) . zip fieldOrder $ myTicket
  where
    isDepartureField = isPrefixOf "departure"
    validTickets = filter (null . flip ticketInvalidValues rules) nearbyTickets
    fieldOrder = determineFieldOrder validTickets rules

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example1" 71 (part1 example1)
         , mkTestCase "part1 input" 22000 (part1 input')
         , let (rs, _, ts) = example2
           in mkTestCase "determineFieldOrder example2"
                         ["row", "class", "seat"]
                         (determineFieldOrder ts rs)
         , mkTestCase "part2 input" 410460648673 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
