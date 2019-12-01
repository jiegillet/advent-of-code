module Day01 where

main :: IO ()
main = do
  masses <- map read . lines <$> readFile "Day01.txt"
  print $ part1 masses
  print $ part2 masses

part1 :: [Double] -> Int
part1 = floor . sum . map fuel

part2 :: [Double] -> Int
part2 = floor . sum . map (sum . tail . takeWhile (>0) . iterate fuel) 

fuel :: Double -> Double
fuel m = fromIntegral $ floor (m / 3) - 2
