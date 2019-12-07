module Day07 where

import IntCode (IntCode, getOutput, mkIntCode)
import Data.List (permutations)

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day07.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode
  print $ part2 intCode

part1 :: IntCode -> Int
part1 code = maximum $ map thruster (permutations [0..4])
  where
    thruster :: [Int] -> Int
    thruster = foldl amplifier 0

    amplifier :: Int -> Int -> Int
    amplifier out phase = last $ getOutput code [phase, out]

part2 :: IntCode -> Int
part2 code = maximum $ map thruster (permutations [5..9])
  where
    thruster :: [Int] -> Int
    thruster [p1, p2, p3, p4, p5] =
      let o1 = getOutput code (p1 : 0 : o5)
          o2 = getOutput code (p2 : o1)
          o3 = getOutput code (p3 : o2)
          o4 = getOutput code (p4 : o3)
          o5 = getOutput code (p5 : o4)
      in last o5
