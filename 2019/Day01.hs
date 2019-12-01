module Day01 where

main :: IO ()
main = do
  masses <- map read . lines <$> readFile "Day01.txt"
  print $ part1 masses
  print $ part2 masses

part1 :: [Int] -> Int
part1 = sum . map fuel

part2 :: [Int] -> Int
part2 = sum . map (sum . tail . takeWhile (>0) . iterate fuel)

fuel :: Int -> Int
fuel m = (m `div` 3) - 2
