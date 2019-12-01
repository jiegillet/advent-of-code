import Data.IntSet (insert, member, singleton)

main :: IO ()
main = do
  txt <- readFile "../input/Day1.txt"
  print . sol1 . map read . words . filter (/='+') $ txt
  print . sol2 . map read . words . filter (/='+') $ txt

sol1 :: [Int] -> Int
sol1 = sum

sol2 :: [Int] -> Int
sol2 = go 0 (singleton 0) . cycle
  where
    go f s (i:is)
      | member f' s = f'
      | otherwise   = go f' (insert f' s) is
      where
        f' = f + i
