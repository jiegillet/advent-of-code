{-# LANGUAGE TupleSections #-}

module Day10 where

import Data.List (maximumBy, sort, groupBy, sortOn, transpose)
import Data.Function (on)
import Data.Ratio (Ratio, (%))
import Data.Set (Set, fromList)

type Position = (Int, Int)

data Direction
  = Center
  | Up
  | Down
  | Diag Int Int (Ratio Int)
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  asteroids <- lines <$> readFile "Day10.txt"
  -- print $ part1 $ lines ex0
  -- print $ part1 $ lines ex1
  -- print $ part1 $ lines ex2
  -- print $ part1 $ lines ex3
  print $ part1 asteroids
  -- print $ part2 $ lines ex0
  -- print $ part2 $ lines ex4
  -- print $ part2 $ lines ex5
  print $ part2 asteroids

lineOfSight :: [[Char]] -> [(Int, Position)]
lineOfSight asteroids = map (\xy -> (count xy, xy)) positions
  where
    positions = concat $ zipWith rows [0..] asteroids
    rows y = map ((, y) . fst) . filter ((=='#') . snd) . zip [0..]

    count xy = length $ fromList $ map (direction xy) positions

    direction (x, y) (x', y')
      | x == x' && y == y' = Center
      | y == y' && x < x'  = Up
      | y == y' && x > x'  = Down
      | otherwise = Diag (signum (x-x')) (signum (y-y')) ((x-x') % (y-y'))

part1 :: [[Char]] -> Int
part1 = (subtract 1) . fst . maximum . lineOfSight

data Dir
  = Self
  | Above
  | Rght (Ratio Int)
  | Below
  | Lft (Ratio Int)
  deriving (Show, Eq, Ord)

part2 :: [[Char]] -> Int
part2 asteroids =
    toInt $
    (!!200) $
    map snd $
    concat $
    transpose $
    map (sortOn radLength) $
    groupBy ((==) `on` fst) $
    sort $
    map (\(_, xy) -> (direction xy, xy)) $
    positions
  where
    positions = lineOfSight asteroids
    (sx, sy) = snd $ maximum $ positions

    radLength (_, (x,y)) = abs (x - sx) + abs (y - sy)
    toInt (x, y) = 100 * x + y

    direction (x, y)
      | x == sx && y == sy = Self
      | x == sx && y < sy  = Above
      | x == sx && y > sy  = Below
      | x > sx = Rght ((y-sy) % (x-sx))
      | x < sx = Lft ((y-sy) % (x-sx))

ex0 = ".#..#\n.....\n#####\n....#\n...##"
ex1 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
ex2 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
ex3 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
ex4 =".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
ex5 = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"
