{-# LANGUAGE TupleSections #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map

-- input = ".#.\n..#\n###"

input = "..#..#..\n#.#...#.\n..#.....\n##....##\n#..#.###\n.#..#...\n###..#..\n....#..#"

main :: IO ()
main = do
  print $ part1 input
  print $ part2 input

data Cell = Active | Inactive deriving (Show, Eq)

toCell :: Char -> Cell
toCell '#' = Active
toCell '.' = Inactive

data Pos = Pos {x :: Integer, y :: Integer, z :: Integer} deriving (Show, Eq, Ord)

neighbors :: Pos -> [Pos]
neighbors (Pos x y z) =
  [Pos i j k | i <- [x -1, x, x + 1], j <- [y -1, y, y + 1], k <- [z -1, z, z + 1], (x, y, z) /= (i, j, k)]

makeState :: String -> Map Pos Cell
makeState = Map.fromList . concat . zipWith toLife [0 ..] . map (zip [0 ..]) . lines
  where
    toLife i line = map (\(j, c) -> (Pos i j 0, toCell c)) line

part1 :: String -> Int
part1 = Map.size . Map.filter (== Active) . (!! 6) . iterate step1 . makeState

step1 :: Map Pos Cell -> Map Pos Cell
step1 grid = Map.mapWithKey evolve $ addNeighbors grid
  where
    evolve pos Active = let c = countNeighbors pos in if c == 2 || c == 3 then Active else Inactive
    evolve pos Inactive = if countNeighbors pos == 3 then Active else Inactive

    countNeighbors = length . filter (== Active) . map (\p -> Map.findWithDefault Inactive p grid) . neighbors

    addNeighbors grid = Map.unionWith const grid aroundGrid
      where
        activePos = Map.keys $ Map.filter (== Active) grid
        aroundGrid = Map.fromList $ map (,Inactive) $ concatMap neighbors activePos

data Pos4 = Pos4 {s :: Integer, t :: Integer, u :: Integer, v :: Integer} deriving (Show, Eq, Ord)

neighbors4 :: Pos4 -> [Pos4]
neighbors4 (Pos4 x y z w) =
  [ Pos4 i j k l
    | i <- [x -1, x, x + 1],
      j <- [y -1, y, y + 1],
      k <- [z -1, z, z + 1],
      l <- [w -1, w, w + 1],
      (x, y, z, w) /= (i, j, k, l)
  ]

makeState4 :: String -> Map Pos4 Cell
makeState4 = Map.fromList . concat . zipWith toLife [0 ..] . map (zip [0 ..]) . lines
  where
    toLife i line = map (\(j, c) -> (Pos4 i j 0 0, toCell c)) line

part2 :: String -> Int
part2 = Map.size . Map.filter (== Active) . (!! 6) . iterate step2 . makeState4

step2 :: Map Pos4 Cell -> Map Pos4 Cell
step2 grid = Map.mapWithKey evolve $ addNeighbors grid
  where
    evolve pos Active = let c = countNeighbors pos in if c == 2 || c == 3 then Active else Inactive
    evolve pos Inactive = if countNeighbors pos == 3 then Active else Inactive

    countNeighbors = length . filter (== Active) . map (\p -> Map.findWithDefault Inactive p grid) . neighbors4

    addNeighbors grid = Map.unionWith const grid aroundGrid
      where
        activePos = Map.keys $ Map.filter (== Active) grid
        aroundGrid = Map.fromList $ map (,Inactive) $ concatMap neighbors4 activePos
