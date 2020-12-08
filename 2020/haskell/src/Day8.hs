{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day8 (test, solve) where

import AoCUtils
import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Bifunctor (bimap)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Instr where
  NOP :: Int -> Instr
  ACC :: Int -> Instr
  JMP :: Int -> Instr

deriving instance Show Instr

parseInstr :: String -> Instr
parseInstr s = i n
  where
    (i, n) = bimap fi fn $ break (== ' ') s
    fi i = case i of
      "nop" -> NOP
      "acc" -> ACC
      "jmp" -> JMP
      _ -> error ("Unknown instruction: " ++ i)
    fn n = case n of
      (' ':'+':ns) -> read ns
      (' ':'-':ns) -> - read ns
      _ -> error ("Unknown n: " ++ n)

data Computer where
  Computer :: { program :: IntMap Instr
              , accum :: Int
              , ip :: Int
              , progSize :: Int
              } -> Computer

deriving instance Show Computer

mkComputer :: [Instr] -> Computer
mkComputer instrs =
  Computer { program = IntMap.fromList (zip [0..] instrs),  accum = 0, ip = 0, progSize = length instrs }

stepComputer :: Computer -> Computer
stepComputer computer@Computer{..} =
  case program IntMap.! ip of
    NOP _ -> computer { ip = ip + 1 }
    ACC n -> computer { ip = ip + 1, accum = accum + n }
    JMP n -> computer { ip = ip + n }

runUntil :: Monad m
         => (Computer -> m (Maybe reason))
         -> Computer
         -> m (Computer, reason)
runUntil p c = p c >>= \case
    Just r  -> pure (c, r)
    Nothing -> runUntil p (stepComputer c)

inInfiniteLoop :: Computer -> State IntSet Bool
inInfiniteLoop computer = do
  seen <- get
  let compIp = ip computer
  if IntSet.member compIp seen
  then pure True
  else put (IntSet.insert compIp seen) >> pure False

isHalted :: Monad m => Computer -> m Bool
isHalted Computer {..} = pure (ip >= progSize)

input :: IO [Instr]
input = map parseInstr . lines <$> readFile "input/day8.txt"

part1 :: [Instr] -> Int
part1 = accum . runUntil' . mkComputer
  where
    runUntil' :: Computer -> Computer
    runUntil' =
      fst
      . flip evalState IntSet.empty
      . runUntil ((boolToMaybe <$>) . inInfiniteLoop)
    boolToMaybe True = Just ()
    boolToMaybe False = Nothing

data HaltReason = InfLoop | Halted deriving Show

part2 :: [Instr] -> Int
part2 instrs = accum $ fst $ head $ dropWhile dropInfLoop computers
  where
    computer = mkComputer instrs
    dropInfLoop (_, InfLoop) = True
    dropInfLoop (_, Halted) = False
    computers = foldr f [] [0..progSize computer]
      where
        f ix comps =
          case (program computer) IntMap.! ix of
            ACC _ -> comps
            NOP n -> runUntil' computer { program = IntMap.insert ix (JMP n) (program computer) } : comps
            JMP n -> runUntil' computer { program = IntMap.insert ix (NOP n) (program computer) } : comps
    runUntil' :: Computer -> (Computer, HaltReason)
    runUntil' = flip evalState IntSet.empty . runUntil p
      where
        p computer = do
          infiniteLoop <- inInfiniteLoop computer
          halted <- isHalted computer
          case (infiniteLoop, halted) of
            (False, False) -> pure $ Nothing
            (True, False)  -> pure $ Just InfLoop
            (False, True)  -> pure $ Just Halted
            _              -> error "Invalid halting"
-- input >>= pure . part2
test = do
  input' <- input
  mkTest [ mkTestCase "part1 input" 1451 (part1 input')
         , mkTestCase "part1 input" 1160 (part2 input')
         ]
-- test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
