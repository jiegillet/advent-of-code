module Main where

import qualified Data.IntMap as Map

main :: IO ()
main = do
  let input = [18, 11, 9, 0, 5, 1]
  print $ part1 input
  print $ part2 input

game :: Int -> [Int] -> Int
game limit input = go (Map.fromList $ zip (init input) [1 ..]) (length input) (last input)
  where
    go history round n
      | round == limit = n
      | otherwise = case history Map.!? n of
        Just age -> go (Map.insert n round history) (round + 1) (round - age)
        Nothing -> go (Map.insert n round history) (round + 1) 0

part1 :: [Int] -> Int
part1 = game 2020

part2 :: [Int] -> Int
part2 = game 30000000
