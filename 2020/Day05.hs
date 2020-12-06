module Day05 where

import Data.List (groupBy, sort)

main :: IO ()
main = do
  seats <- lines <$> readFile "Day05.txt"
  print $ part1 seats
  print $ part2 seats

toId :: String -> Int
toId = foldl go 0
  where
    go n 'F' = 2 * n
    go n 'B' = 2 * n + 1
    go n 'L' = 2 * n
    go n 'R' = 2 * n + 1

part1 :: [String] -> Int
part1 = maximum . map toId

part2 :: [String] -> Int
part2 = findHole . sort . map toId
  where
    findHole (a : b : rest)
      | b - a == 2 = a + 1
      | otherwise = findHole (b : rest)
