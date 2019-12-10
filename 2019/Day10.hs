module Day10 where

import           Data.Ratio
import           Data.Set   (Set)
import qualified Data.Set   as Set

data Direction
  = Up
  | Down
  | Center
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
  -- print $ part2 masses

part1 :: [[Char]] -> Int
part1 asteroids = maximum view
  where
    locations = concat $ zipWith rows [0..] asteroids

    rows y = map (toCoord y) . filter ((=='#') . snd) . zip [0..]

    toCoord y (x, _) = (x, y)

    view = map count locations
    count xy = (subtract 1) $ length $ Set.fromList $ map (direction xy) locations

    direction (x, y) (x', y')
      | x == x' && y == y' = Center
      | y == y' && x < x'  = Up
      | y == y' && x > x'  = Down
      | otherwise = Diag (signum (x-x')) (signum (y-y')) ((x-x') % (y-y'))

-- part2 :: [Int] -> Int
-- part2 = sum . map (sum . tail . takeWhile (>0) . iterate fuel)

ex0 = ".#..#\n.....\n#####\n....#\n...##"
ex1 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
ex2 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
ex3 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
