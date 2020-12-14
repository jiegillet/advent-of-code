module Main where

main :: IO ()
main = do
  [[timestamp], buses] <- map (splitBy ',') . lines <$> readFile "Day13.txt"
  print $ part1 (read timestamp) buses
  print $ part2 buses

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c (',' : s) = splitBy c s
splitBy c s = let (a, b) = span (/= c) s in a : splitBy c b

part1 :: Int -> [String] -> Int
part1 timestamp buses' = uncurry (*) $ minimum $ zip (map (\b -> b - mod timestamp b) buses) buses
  where
    buses = map read $ filter (/= "x") buses'

part2 :: [String] -> Int
part2 buses' = fst $ foldl1 multiples buses
  where
    buses = map (fmap read) $ filter ((/= "x") . snd) $ zip [0 ..] buses'

    minMultiple (t1, n1) (t2, n2) = head $ filter (\k -> n2 - mod (k * n1) n2 == mod (t1 + t2) n2) [1 ..]

    multiples (t1, n1) (t2, n2) = let dt = minMultiple (t1, n1) (t2, n2) in (t1 + dt * n1, n2 * n1)