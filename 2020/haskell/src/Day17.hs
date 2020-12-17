{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Day17 (test, solve) where

import           AoCUtils
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import           Data.List
import           GHC.Generics (Generic)

data ActiveState = Active | Inactive deriving (Eq, Show)

isActive, isInactive :: ActiveState -> Bool
isActive Active = True
isActive Inactive = False
isInactive = not . isActive

data Coord3 =
  Coord3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Generic, Hashable, Show)

data Coord4 =
  Coord4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Generic, Hashable, Show)

instance Semigroup Coord3 where
  Coord3 x y z <> Coord3 x' y' z' = Coord3 (x + x') (y + y') (z + z')

instance Monoid Coord3 where
  mempty = Coord3 0 0 0

instance Semigroup Coord4 where
  Coord4 x y z w <> Coord4 x' y' z' w' = Coord4 (x + x') (y + y') (z + z') (w + w')

instance Monoid Coord4 where
  mempty = Coord4 0 0 0 0

class Coord a where
  neighbors :: a -> [a]
  setR :: Int -> a -> a
  setC :: Int -> a -> a

instance Coord Coord3 where
  neighbors coord =
    [coord <> n | r <- [1, 0, -1], c <- [1, 0, -1], z <- [1, 0, -1]
                , not (r == 0 && c == 0 && z == 0)
                , let n = Coord3 r c z
                ]
  setR r (Coord3 _ c z) = Coord3 r c z
  {-# INLINE setR #-}
  setC c (Coord3 r _ z) = Coord3 r c z
  {-# INLINE setC #-}

instance Coord Coord4 where
  neighbors coord =
    [coord <> n | r <- [1, 0, -1], c <- [1, 0, -1], z <- [1, 0, -1], w <- [1, 0, -1]
                , not (r == 0 && c == 0 && z == 0 && w == 0)
                , let n = Coord4 r c z w
                ]
  setR r (Coord4 _ c z w) = Coord4 r c z w
  {-# INLINE setR #-}
  setC c (Coord4 r _ z w) = Coord4 r c z w
  {-# INLINE setC #-}

type Dimension coord = HashMap coord ActiveState

type DimensionC c = (Coord c, Hashable c, Eq c, Monoid c)

parseDimension :: DimensionC coord => String -> Dimension coord
parseDimension = HashMap.fromList . concatMap (uncurry parseLine) . zip [0..] . lines
  where
    parseLine r = zipWith (parseChar r) [0..]
    parseChar r c '#' = (setC c $ setR r mempty, Active)
    parseChar r c '.' = (setC c $ setR r mempty, Inactive)
    parseChar _ _ char = error $ "Invalid char: " ++ [char]

example :: DimensionC coord => Dimension coord
example = parseDimension ".#.\n\
\..#\n\
\###"

input :: (Coord coord, Eq coord, Monoid coord, Hashable coord) => IO (Dimension coord)
input = parseDimension <$> readFile "input/day17.txt"

step :: DimensionC c => Dimension c -> Dimension c
step dimension = HashMap.fromList $ map step' $ HashSet.toList allCoords
  where
    allCoords = foldl' f HashSet.empty $ HashMap.keys dimension
      where f s = foldl' (flip HashSet.insert) s . neighbors
    step' coord
      | isActive activeState && (numActiveNeighbors == 2 || numActiveNeighbors == 3) =
          (coord, Active)
      | isInactive activeState && numActiveNeighbors == 3 =
          (coord, Active)
      | otherwise = (coord, Inactive)
      where
        activeState = HashMap.findWithDefault Inactive coord dimension
        activeNeighbors =
          filter (isActive . snd)
          $ map (\c -> (c, HashMap.findWithDefault Inactive c dimension))
          $ neighbors coord
        numActiveNeighbors = length activeNeighbors

solve' :: DimensionC coord => Dimension coord -> Int
solve' = length . filter isActive . HashMap.elems . (!! 6) . iterate step

part1 :: Dimension Coord3 -> Int
part1 = solve'

part2 :: Dimension Coord4 -> Int
part2 = solve'

test :: IO ()
test = do
  input3 <- input
  input4 <- input
  mkTest [ mkTestCase "part1 example" 112 (part1 example)
         , mkTestCase "part1 input" 263 (part1 input3)
         , mkTestCase "part2 example" 848 (part2 example)
         , mkTestCase "part2 input" 1680 (part2 input4)
         ]
--test
solve :: IO ()
solve = do
  input3 <- input
  input4 <- input
  print (part1 input3)
  print (part2 input4)
