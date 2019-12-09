module Day09 where

import           IntCode (IntCode, getOutput, mkIntCode)

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day09.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode
  print $ part2 intCode

part1 :: IntCode -> [Integer]
part1 code = getOutput code [1]

part2 :: IntCode -> [Integer]
part2 code = getOutput code [2]
