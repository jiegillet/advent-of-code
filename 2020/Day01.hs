module Day01 where

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  ns <- map read . lines <$> readFile "Day01.txt"
  print $ part1 ns
  print $ part2 ns

part1 :: [Int] -> Int
part1 ns =
  let set = Set.fromList ns
      sol = head $ filter (\n -> Set.member (2020 - n) set) ns
   in sol * (2020 - sol)

part2 :: [Int] -> Int
part2 ns =
  let m = Map.fromList [(a + b, (a, b)) | a <- ns, b <- ns]
      x = head $ filter (\n -> Map.member (2020 - n) m) ns
      (y, z) = m Map.! (2020 - x)
   in x * y * z