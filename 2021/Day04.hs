module Day04 where

import Control.Monad
import Data.List (delete, maximumBy, minimumBy, transpose, (\\))

main :: IO ()
main = do
  (ballLine : lines) <- lines <$> readFile "Day04.txt"
  let grids = parseGrids lines []
      balls = read $ "[" ++ ballLine ++ "]"
  print $ part1 balls grids
  print $ part2 balls grids

type Grid = [[Int]]

bingoSize :: Int
bingoSize = 5

parseGrids :: [String] -> [Grid] -> [Grid]
parseGrids [] grids = reverse grids
parseGrids lines grids = parseGrids (drop (bingoSize + 1) lines) (grid : grids)
  where
    grid = map (map read . words) $ take bingoSize $ tail lines

part1 :: [Int] -> [Grid] -> Int
part1 balls grids = snd $ maximum $ map (finalState balls) grids

part2 :: [Int] -> [Grid] -> Int
part2 balls grids = snd $ minimum $ map (finalState balls) grids

finalState :: [Int] -> Grid -> (Int, Int)
finalState balls grid = (ballsLeft, score)
  where
    ballsLeft = maximum $ map (checkRow balls) rows
    (_, called) = splitAt ballsLeft (reverse balls)
    score = sum (concat grid \\ called) * head called
    rows = grid ++ transpose grid

    checkRow balls [] = length balls
    checkRow [] _ = 0
    checkRow (ball : balls) row = checkRow balls (filter (ball /=) row)
