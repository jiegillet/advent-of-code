module Day03 where

main :: IO ()
main = do
  slope <- lines <$> readFile "Day03.txt"
  -- let slope = tree_ex
  print $ numberOfTrees 3 1 (length $ head slope) slope
  print $ product [numberOfTrees r d (length $ head slope) slope | (r, d) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]

numberOfTrees :: Int -> Int -> Int -> [[Char]] -> Int
numberOfTrees right down size = snd . foldl go (- right, 0) . every down
  where
    go (pos, n) trees =
      let pos' = (pos + right) `mod` size
       in if trees !! pos' == '.' then (pos', n) else (pos', n + 1)

every :: Int -> [a] -> [a]
every _ [] = []
every k (x : xs) = x : every k (drop (k -1) xs)

tree_ex = lines "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"