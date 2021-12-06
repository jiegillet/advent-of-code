module Day06 where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "Day06.txt"
  let population = read $ "[" ++ input ++ "]"
  print $ reproduce 80 population
  print $ reproduce 256 population

reproduce :: Int -> [Int] -> Int
reproduce days = Map.foldr (+) 0 . (!! days) . iterate (Map.foldrWithKey day Map.empty) . prepare
  where
    prepare :: [Int] -> Map Int Int
    prepare = Map.fromListWith (+) . flip zip (repeat 1)

    day :: Int -> Int -> Map Int Int -> Map Int Int
    day 0 n = Map.insertWith (+) 8 n . Map.insertWith (+) 6 n
    day d n = Map.insertWith (+) (d - 1) n
