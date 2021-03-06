module Day05 where

import IntCode (IntCode, getOutput, mkIntCode)

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day05.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode [1]
  print $ part2 intCode [5]

part1 :: IntCode -> [Integer] -> [Integer]
part1 = getOutput

part2 :: IntCode -> [Integer] -> [Integer]
part2 = getOutput
