{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Day22 (test, solve) where

import AoCUtils
import Data.List.Split (splitOn)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Singletons
import Data.Singletons.TH

type Deck = [Int]
type Decks = (Deck, Deck)

data Player = Player1 | Player2 deriving (Show, Eq)
genSingletons [''Player]

data GameStatus where
  Won :: Player -> GameStatus
  Ongoing :: GameStatus
genSingletons [''GameStatus]

data GameState (a :: GameStatus) =
  GameState
    { gsDecks :: Decks
    , gsPrevDecks :: HashSet Decks
    } deriving (Show)

newGameState :: Decks -> GameState 'Ongoing
newGameState decks = GameState decks HashSet.empty

data SomeGameState where
  SomeGameState :: Sing s -> GameState s -> SomeGameState

input :: IO Decks
input =
  (\[a, b] -> (a, b))
  . map (map read . tail . lines)
  . splitOn "\n\n"
  <$> readFile "input/day22.txt"

example1 :: Decks
example1 = ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])

example2a :: Decks
example2a = ([43, 19], [2, 29, 14])

example2b :: Decks
example2b = ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])

calcDecks'Part1 :: GameState 'Ongoing -> Decks
calcDecks'Part1 (GameState (a:as', b:bs') _) =
  if a > b then
    (as' ++ [a, b], bs')
  else
    (as', bs' ++ [b, a])

calcDecks'Part2 :: GameState 'Ongoing -> Decks
calcDecks'Part2 (GameState (a:as', b:bs') _) =
  if a == length as'' && b == length bs'' then
    case playUntilWon calcDecks'Part2 (newGameState (as'', bs'')) (`withSingI` winningPlayer) of
      Player1 -> (as' ++ [a, b], bs')
      Player2 -> (as', bs' ++ [b, a])
  else
    if a > b then
      (as' ++ [a, b], bs')
    else
      (as', bs' ++ [b, a])
  where
    as'' = take a as'
    bs'' = take b bs'

playRound :: (GameState 'Ongoing -> Decks) -> GameState 'Ongoing -> SomeGameState
playRound _ (GameState decks prevDecks)
  | HashSet.member decks prevDecks = SomeGameState (SWon SPlayer1) (GameState decks prevDecks)
playRound calcDecks' gs@(GameState decks prevDecks) =
  case decks' of
    (_, []) -> SomeGameState (SWon SPlayer1) (GameState decks' prevDecks')
    ([], _) -> SomeGameState (SWon SPlayer2) (GameState decks' prevDecks')
    _       -> SomeGameState SOngoing (gs { gsDecks = decks', gsPrevDecks = prevDecks' })
  where
    decks' = calcDecks' gs
    prevDecks' = HashSet.insert decks prevDecks

playUntilWon :: (GameState 'Ongoing -> Decks)
             -> GameState 'Ongoing
             -> (forall p. Sing p -> GameState ('Won p) -> r)
             -> r
playUntilWon calcDecks' gs f =
  case playRound calcDecks' gs of
    SomeGameState (SWon p) gs' -> f p gs'
    SomeGameState SOngoing gs' -> playUntilWon calcDecks' gs' f

winningDeck :: forall p. SingI p => GameState ('Won p) -> Deck
winningDeck gs =
  case sing @p of
    SPlayer1 -> fst $ gsDecks gs
    SPlayer2 -> snd $ gsDecks gs

deckScore :: Deck -> Int
deckScore deck = sum $ zipWith (*) (reverse deck) [1..]

winningScore :: SingI p => GameState ('Won p) -> Int
winningScore gs = deckScore (winningDeck gs)

winningPlayer :: forall p. SingI p => GameState ('Won p) -> Player
winningPlayer _ = fromSing $ sing @p

part1 :: Decks -> Int
part1 decks = playUntilWon calcDecks'Part1 (newGameState decks) (`withSingI` winningScore)

part2 :: Decks -> Int
part2 decks = playUntilWon calcDecks'Part2 (newGameState decks) (`withSingI` winningScore)

test :: IO ()
test = do
  input' <- input
  mkTest [ mkTestCase "part1 example" 306 (part1 example1)
         , mkTestCase "part1 input" 35299 (part1 input')
         , mkTestCase "part2 infinite example" Player1 (playUntilWon calcDecks'Part2 (newGameState example2a) (`withSingI` winningPlayer))
         , mkTestCase "part2 example" 291 (part2 example2b)
         , mkTestCase "part2 input" 33266 (part2 input')
         ]

solve :: IO ()
solve = do
  input' <- input
  print $ part1 input'
  print $ part2 input'
