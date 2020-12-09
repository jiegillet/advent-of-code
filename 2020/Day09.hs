module Day09 where

import qualified Data.Set as Set

main :: IO ()
main = do
  numbers <- map read . lines <$> readFile "Day09.txt"
  print $ part1 numbers
  print $ part2 numbers

part1 :: [Int] -> (Int, Int)
part1 n = isPairSum 0 (drop 25 n) n
  where
    isPairSum index (candidate : candidates) numbers
      | any (\n -> Set.member (candidate - n) set) numbers = isPairSum (index + 1) candidates (tail numbers)
      | otherwise = (index, candidate)
      where
        set = Set.fromList $ take 25 numbers

-- part2 :: [Int] -> Int
part2 numbers = go numbers
  where
    (_, invalid) = part1 numbers
    go (smallest : rest)
      | sum < invalid = go rest
      | otherwise = (smallest, biggest)
      where
        (biggest, sum) = check smallest smallest rest
        check sum big (n : ns)
          | sum + n > invalid = (big, sum)
          | otherwise = check (sum + n) n ns