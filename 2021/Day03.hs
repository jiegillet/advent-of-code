module Day03 where

import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort)

main :: IO ()
main = do
  --  let ns = test
  ns <- lines <$> readFile "Day03.txt"
  print $ part1 ns
  print $ part2 ns

part1 :: [String] -> Int
part1 = gammaEpsilon . foldr (zipWith (+) . map toValue) (repeat 0)

toValue :: Char -> Int
toValue '0' = -1
toValue '1' = 1

gammaEpsilon :: [Int] -> Int
gammaEpsilon numbers = gamma * epsilon
  where
    gamma = foldl accum 0 numbers
    epsilon = 2 ^ length numbers - 1 - gamma
    accum tot n
      | n > 0 = 2 * tot + 1
      | n < 0 = 2 * tot

part2 :: [String] -> Int
part2 input = filterInput maximumBy 0 input * filterInput minimumBy 0 input
  where
    filterInput _ _ [number] = foldl accum 0 number
    filterInput mostBy n numbers = filterInput mostBy (n + 1) (filter ((== common) . (!! n)) numbers)
      where
        common = head $ mostBy (compare `on` length) $ group $ sort $ map (!! n) numbers

    accum tot '0' = 2 * tot
    accum tot '1' = 2 * tot + 1

test :: [String]
test =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]