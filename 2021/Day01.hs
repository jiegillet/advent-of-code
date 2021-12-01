module Day01 where

main :: IO ()
main = do
  ns <- map read . lines <$> readFile "Day01.txt"
  print $ part1 ns
  print $ part2 ns

part1 :: [Int] -> Int
--part1 = length . filter id . (zipWith (<=) <*> tail)
part1 depth = length $ filter id $ zipWith (<) depth (tail depth)

part2 :: [Int] -> Int
--part2 = length . filter id . (zipWith (<) <*> (tail . tail . tail))
part2 depth = length $ filter id $ zipWith (<) depth (tail $ tail $ tail depth)
