{-# LANGUAGE LambdaCase#-}

module Day14 (test, solve) where

import AoCUtils
import Data.Bits (clearBit, setBit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Word

type Mask = [(Int, Char)]
type Memory = IntMap Word64

data Instr =
  SetMask Mask | MaskValue { addr :: Int, value :: Word64 }
  deriving Show

data Computer =
  Computer { mask :: Mask
           , memory :: Memory
           } deriving Show

parseMask :: String -> Mask
parseMask = zip [0..] . reverse

parseInstr :: String -> Instr
parseInstr s =
  case s of
    'm':'a':'s':'k':' ':'=':' ':xs -> SetMask $ parseMask xs
    'm':'e':'m':'[':xs ->
      let (addr, val) = break (== ']') xs
      in MaskValue (read addr) (read $ drop 4 val)
    _ -> error $ "Invalid instruction line: " ++ s

example1 :: [Instr]
example1 =
  map parseInstr [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 , "mem[8] = 11"
                 , "mem[7] = 101"
                 , "mem[8] = 0"
                 ]

example2 :: [Instr]
example2 =
  map parseInstr [ "mask = 000000000000000000000000000000X1001X"
                 , "mem[42] = 100"
                 , "mask = 00000000000000000000000000000000X0XX"
                 , "mem[26] = 1"
                 ]

input :: IO [Instr]
input = map parseInstr . lines <$> readFile "input/day14.txt"

maskValue :: Word64 -> Mask -> Word64
maskValue = foldl' updateValue
  where
    updateValue value (idx, char) =
      case char of
        'X' -> value
        '1' -> setBit value idx
        '0' -> clearBit value idx
        _   -> error $ "Invalid char: " ++ [char]

execInstrPart1 :: Computer -> Instr -> Computer
execInstrPart1 comp@(Computer mask memory) =
  \case
    MaskValue addr value ->
      let memory' = IntMap.insert addr (maskValue value mask) memory
      in comp { memory = memory'}
    SetMask mask' -> comp { mask = mask' }

part1 :: [Instr] -> Word64
part1 instrs = foldl' (+) 0 $ (IntMap.elems $ memory computer)
  where computer = foldl' execInstrPart1 (Computer [] IntMap.empty) instrs

maskAddress :: Int -> Mask -> [Int]
maskAddress addr = go [addr]
  where
    go :: [Int] -> Mask -> [Int]
    go acc [] = acc
    go acc ((idx, char):mask) =
      case char of
        '0' -> go acc mask
        '1' -> go (map (flip setBit idx) acc) mask
        'X' -> go (map (flip setBit idx) acc ++ map (flip clearBit idx) acc) mask
        _   -> error $ "Invalid char: " ++ [char]

execInstrPart2 :: Computer -> Instr -> Computer
execInstrPart2 comp@(Computer mask memory) =
  \case
    MaskValue addr value ->
      let memory' = foldl' (\m addr -> IntMap.insert addr value m) memory $ maskAddress addr mask
      in comp { memory = memory' }
    SetMask mask' -> comp { mask = mask' }

part2 :: [Instr] -> Word64
part2 instrs = foldl' (+) 0 $ (IntMap.elems $ memory computer)
  where computer = foldl' execInstrPart2 (Computer [] IntMap.empty) instrs

test = do
  input' <- input
  mkTest [ mkTestCase "part1 example1" 165 (part1 example1)
         , mkTestCase "part1 input" 11179633149677 (part1 input')
         , mkTestCase "part2 example2" 208 (part2 example2)
         , mkTestCase "part2 input" 4822600194774 (part2 input')
         ]
--test
solve = do
  input' <- input
  print (part1 input')
  print (part2 input')
--solve
