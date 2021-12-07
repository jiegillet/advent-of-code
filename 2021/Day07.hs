module Day07 where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "Day07.txt"
  let positions = read $ "[" ++ input ++ "]"
  print $ part1 positions
  print $ part2 positions

part1 :: [Int] -> Int
part1 positions = minimum $ map cost [minimum positions .. maximum positions]
  where
    cost n = foldr (\p -> (+) (abs (p - n))) 0 positions

part2 :: [Int] -> Int
part2 positions = minimum $ map cost [minimum positions .. maximum positions]
  where
    cost n = foldr (\p -> (+) (abs (p - n) * (abs (p - n) + 1) `div` 2)) 0 positions
