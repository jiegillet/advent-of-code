module Day10 where

import qualified Data.List as L

main :: IO ()
main = do
  inputs <- lines <$> readFile "Day10.txt"
  print $ part1 inputs
  print $ part2 inputs

part1 :: [String] -> Int
part1 = sum . map (go [])
  where
    go _ [] = 0
    go stack (braket : rest) =
      if braket `elem` "([{<"
        then go (braket : stack) rest
        else case stack of
          [] -> corruptPoints braket
          (b : bs) -> if match b braket then go bs rest else corruptPoints braket

corruptPoints :: Char -> Int
corruptPoints ')' = 3
corruptPoints ']' = 57
corruptPoints '}' = 1197
corruptPoints '>' = 25137

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match _ _ = False

part2 :: [String] -> Int
part2 = midScore . dropWhile (== 0) . L.sort . map (score [])
  where
    score stack [] = calculatePoints stack
    score stack (braket : rest) =
      if braket `elem` "([{<"
        then score (braket : stack) rest
        else case stack of
          [] -> 0
          (b : bs) -> if match b braket then score bs rest else 0

    calculatePoints = foldl (\total braket -> 5 * total + incompletePoints braket) 0

    midScore scores = scores !! (length scores `div` 2)

incompletePoints :: Char -> Int
incompletePoints '(' = 1
incompletePoints '[' = 2
incompletePoints '{' = 3
incompletePoints '<' = 4