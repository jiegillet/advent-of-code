module Main where

import Data.List (sort)

main :: IO ()
main = do
  adapters <- map read . lines <$> readFile "Day10.txt"
  print $ part1 adapters
  print $ part2 adapters

part1 :: [Integer] -> Integer
part1 = uncurry (*) . count (0, 1) . (0 :) . sort
  where
    count (ones, threes) (x : y : rest)
      | y - x == 1 = count (ones + 1, threes) (y : rest)
      | y - x == 3 = count (ones, threes + 1) (y : rest)
      | otherwise = error "Are there any 2 jolt differences? Nope :)"
    count x _ = x

part2 :: [Integer] -> Integer
part2 = product . map count . map length . groupByDiff [[]] . (-1 :) . (0 :) . sort
  where
    groupByDiff (group : groups) (x : y : rest)
      | y - x == 1 = groupByDiff ((y : group) : groups) (y : rest)
      | y - x == 3 = groupByDiff ([y] : group : groups) (y : rest)
      | otherwise = groupByDiff (group : groups) (y : rest)
    groupByDiff x _ = x

    count 1 = 1
    count 2 = 1
    count 3 = 2
    count 4 = 4
    count 5 = 7

ex = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

ex2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]