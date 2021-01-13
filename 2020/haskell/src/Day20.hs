{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day20 (test, solve) where

import AoCUtils
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Borders =
  Borders { top :: [Char]
          , bottom :: [Char]
          , left :: [Char]
          , right :: [Char]
          } deriving (Eq, Ord, Show)

bordersToSet :: Borders -> Set String
bordersToSet bs = Set.fromList [top bs, bottom bs, left bs, right bs]

data TileOrientation =
  TO { tile :: [String], borders :: Borders }
  deriving (Eq, Ord, Show)

data Tile =
  Tile { tileId :: !Int
       , orientations :: Set TileOrientation
       , size :: Int
       } deriving (Show)

instance Eq Tile where
  (==) = (==) `on` tileId

instance Ord Tile where
  (<=) = (<=) `on` tileId

parseTile :: String -> Tile
parseTile s = Tile {..}
  where
    (header:chars) = lines s
    tileId = read . init . (!! 1) . words $ header
    orientations = Set.fromList $ map (TO <*> calcBorders) $ calcOrientations chars
    size = length chars

calcOrientations :: [[a]] -> [[[a]]]
calcOrientations xs = rotations ++ map reverse rotations
  where
    rotations = map (\f -> f xs) [id, rotate, rotate . rotate, rotate . rotate . rotate]
    rotate = transpose . reverse

calcBorders :: [String] -> Borders
calcBorders tile =
  Borders { top = head tile
          , bottom = last tile
          , left = map head tile
          , right = map last tile
          }

borderingTiles :: [Tile] -> Tile -> [Tile]
borderingTiles [] _ = []
borderingTiles (tile:tiles) tile0
  | tileId tile == tileId tile0 = borderingTiles tiles tile0
  | not (null ((Set.intersection `on` (Set.unions . Set.map (bordersToSet . borders') . orientations)) tile tile0)) =
      tile : borderingTiles tiles tile0
  | otherwise = borderingTiles tiles tile0
  where
    borders' = borders :: TileOrientation -> Borders

data Analysis =
  Analysis { corners :: Set Tile
           , borders :: Set Tile
           , inners :: Set Tile
           , imageSize :: Int
           } deriving (Show)

runAnalysis :: [Tile] -> Analysis
runAnalysis tiles = Analysis corners borders inners imageSize
  where
    neighbors' = foldl' f Map.empty tiles
      where f m t = Map.insert t (Set.fromList (borderingTiles tiles t)) m
    (corners, borders, inners, _) =
      foldl' f (Set.empty, Set.empty, Set.empty, Map.empty) $ Map.assocs neighbors'
      where
        f (cs, bs, is, ns) (t, ns') =
          case Set.size ns' of
            2 -> (Set.insert t cs, bs, is, Map.insert (tileId t) ns' ns)
            3 -> (cs, Set.insert t bs, is, Map.insert (tileId t) ns' ns)
            _ -> (cs, bs, Set.insert t is, Map.insert (tileId t) ns' ns)
    imageSize = floor . sqrt . fromIntegral . length $ tiles

data Image =
  Image { grid :: Map (Int, Int) TileOrientation
        , size :: Int
        } deriving (Show)

emptyImage :: Int -> Image
emptyImage n = Image { grid = Map.empty, size = n }

solveImage :: Analysis -> [Image]
solveImage Analysis {..} =
  let cs = [ (0, 0), (0, pred imageSize), (pred imageSize, 0)
           , (pred imageSize, pred imageSize) ]
      bs = [(0, c) | c <- [1 .. pred (pred imageSize)]] ++
           [(pred imageSize, c) | c <- [1 .. pred (pred imageSize)]] ++
           [(r, 0) | r <- [1 .. pred (pred imageSize)]] ++
           [(r, pred imageSize) | r <- [1 .. pred (pred imageSize)]]
      rest = [(r, c) | r <- [1 .. pred (pred imageSize)], c <- [1 .. (pred (pred imageSize))]]
      locs = cs ++ bs ++ rest
  in go (corners, borders, inners) locs (emptyImage imageSize)
  where
    go :: (Set Tile, Set Tile, Set Tile) -> [(Int, Int)] -> Image -> [Image]
    go (cs, bs, is) locs image
      | Set.null cs && Set.null bs && Set.null is && null locs = [image]
      | Set.null cs && Set.null bs = do
          (t, image') <- go' is (head locs) image
          go (cs, bs, Set.delete t is) (tail locs) image'
      | Set.null cs = do
          (t, image') <- go' bs (head locs) image
          go (cs, Set.delete t bs, is) (tail locs) image'
      | otherwise = do
          (t, image') <- go' cs (head locs) image
          go (Set.delete t cs, bs, is) (tail locs) image'
    go' tiles loc image = do
      t <- toList tiles
      o <- toList $ orientations t
      image' <- toList $ place image loc o
      pure (t, image')

place :: Image -> (Int, Int) -> TileOrientation -> Maybe Image
place image loc@(r, c) t = --trace ("place: " ++ show (loc, Map.size $ grid image)) $
  if top' == maybe top' (bottom . borders') (Map.lookup (r - 1, c) (grid image)) &&
     bottom' == maybe bottom' (top . borders') (Map.lookup (r + 1, c) (grid image)) &&
     right' == maybe right' (left . borders') (Map.lookup (r, c + 1) (grid image)) &&
     left' == maybe left' (right . borders') (Map.lookup (r, c - 1) (grid image))
  then Just $ image { grid = Map.insert loc t (grid image) }
  else Nothing
  where
    borders' = borders :: TileOrientation -> Borders
    top' = top (borders' t)
    bottom' = bottom (borders' t)
    right' = right (borders' t)
    left' = left (borders' t)

input :: IO [Tile]
input = map parseTile . splitOn "\n\n" <$> readFile "input/day20.txt"

part1 :: Analysis -> Int
part1 = product . map tileId . toList . corners

imageToList :: Image -> [[TileOrientation]]
imageToList Image{..} = [[grid Map.! (r, c) | c <- [0 .. pred size]] | r <- [0 .. pred size]]

stripBorders :: TileOrientation -> TileOrientation
stripBorders to = to { tile = map (init . tail) . init . tail $ tile to }

imageRowToStrings :: [TileOrientation] -> [String]
imageRowToStrings os
  | null (tile (head os)) = []
  | otherwise =
      concatMap (head . tile) os : imageRowToStrings (map (\o -> o { tile = tail (tile o) }) os)

seaMonster :: [String]
seaMonster =
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   " ]

isSeaMonster :: [String] -> Bool
isSeaMonster ls =
  (length (head ls) >= length (head seaMonster)) &&
  all f (zipWith zip seaMonster ls)
  where
    f = all (\(a, b) -> a /= '#' || b == '#')

countSeaMonsters :: [String] -> Int
countSeaMonsters [_, _] = 0
countSeaMonsters ls = go (take 3 ls) + countSeaMonsters (tail ls)
  where
    go [[], [], []] = 0
    go ls' = bool 0 1 (isSeaMonster ls') + go (map (drop 1) ls')

part2 :: Analysis -> Int
part2 analysis = numHashes - numSeaMonsters * 15
  where
    numSeaMonsters = head $ filter (/= 0) $ map countSeaMonsters images
    numHashes = length $ filter (== '#') $ concat $ head images
    images =
      calcOrientations $ concatMap (imageRowToStrings . map stripBorders) $ imageToList $ head $ solveImage analysis

test :: IO ()
test = do
  analysis <- runAnalysis <$> input
  mkTest [ mkTestCase "part1 input" 5966506063747 (part1 analysis)
         , mkTestCase "part2 input" 1714 (part2 analysis)
         ]

solve :: IO ()
solve = do
  analysis <- runAnalysis <$> input
  print (part1 analysis)
  print (part2 analysis)
