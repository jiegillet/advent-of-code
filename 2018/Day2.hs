import qualified Data.Map as M

main :: IO ()
main = do
  txt <- readFile "../input/Day2.txt"
  print . checksum . words $ txt
  print . prototype . words $ txt

checksum :: [String] -> Int
checksum = (\(a, b) -> a * b) . foldr check (0, 0)
  where
    check x (two, three) = (two + a, three + b)
      where m = M.fromListWith (+) $ zip x (repeat 1)
            a = if null $ M.filter (==2) m then 0 else 1
            b = if null $ M.filter (==3) m then 0 else 1

prototype :: [String] -> String
prototype s = uncurry out . head . filter diff $ (,) <$> s <*> s
  where
    diff (a, b) = (length $ filter id $ zipWith (/=) a b) == 1
    out a = map fst . filter (uncurry (==)) . zip a
