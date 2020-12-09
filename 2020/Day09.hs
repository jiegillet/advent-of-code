module Day09 where

import qualified Data.Set as Set

main :: IO ()
main = do
  numbers <- map read . lines <$> readFile "Day09.txt"
  print $ part1 numbers
  print $ part2 numbers

part1 :: [Int] -> Int
part1 n = isPairSum 0 (drop 25 n) n
  where
    isPairSum index (candidate : candidates) numbers
      | any (\n -> Set.member (candidate - n) set) numbers = isPairSum (index + 1) candidates (tail numbers)
      | otherwise = candidate
      where
        set = Set.fromList $ take 25 numbers

part2 :: [Int] -> Int
part2 numbers = go numbers
  where
    invalid = part1 numbers
    go (low : rest)
      | sum < invalid = go rest
      | otherwise = minimum block + maximum block
      where
        (block, sum) = check low [low] rest
        check sum block (n : ns)
          | sum + n > invalid = (block, sum)
          | otherwise = check (sum + n) (n : block) ns