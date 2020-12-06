module Day06 where

import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Set as Set

main :: IO ()
main = do
  answers <- readFile "Day06.txt"
  print $ part1 ex
  print $ part1 answers
  print $ part2 ex
  print $ part2 answers

part1 :: String -> Int
part1 = sum . map any . groupBy ((==) `on` null) . lines
  where
    any = Set.size . Set.fromList . concat

part2 :: String -> Int
part2 = sum . map all . groupBy ((==) `on` null) . lines
  where
    all = Set.size . foldr1 Set.intersection . map Set.fromList

ex = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"