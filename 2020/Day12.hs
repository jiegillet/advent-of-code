{-# LANGUAGE DeriveFunctor #-}

module Main where

import Prelude hiding (Either (..))

main :: IO ()
main = do
  dirs <- map readDir . lines <$> readFile "Day12.txt"
  --   print $ part1 $ map readDir $ lines ex
  print $ part1 dirs
  --   print $ part2 $ map readDir $ lines ex
  print $ part2 dirs

data Dir a
  = North a
  | South a
  | West a
  | East a
  | Left a
  | Right a
  | Forward a
  deriving (Show, Functor)

readDir :: String -> Dir Int
readDir ('N' : int) = North $ read int
readDir ('S' : int) = South $ read int
readDir ('W' : int) = West $ read int
readDir ('E' : int) = East $ read int
readDir ('L' : int) = Left $ read int
readDir ('R' : int) = Right $ read int
readDir ('F' : int) = Forward $ read int

part1 :: [Dir Int] -> Int
part1 = manhattan . fst . foldl go ((0, 0), East 0)
  where
    go ((i, j), d) (North k) = ((i + k, j), d)
    go ((i, j), d) (South k) = ((i - k, j), d)
    go ((i, j), d) (West k) = ((i, j - k), d)
    go ((i, j), d) (East k) = ((i, j + k), d)
    go ((i, j), d) (Left k) = ((i, j), left k d)
    go ((i, j), d) (Right k) = ((i, j), left (360 - k) d)
    go ((i, j), d) (Forward k) = go ((i, j), d) (fmap (const k) d)

    left 0 d = d
    left deg (West d) = left (deg - 90) (South d)
    left deg (South d) = left (deg - 90) (East d)
    left deg (East d) = left (deg - 90) (North d)
    left deg (North d) = left (deg - 90) (West d)

    manhattan (i, j) = abs i + abs j

part2 :: [Dir Int] -> Int
part2 = manhattan . fst . foldl go ((0, 0), (1, 10))
  where
    go (ship, (i, j)) (North k) = (ship, (i + k, j))
    go (ship, (i, j)) (South k) = (ship, (i - k, j))
    go (ship, (i, j)) (West k) = (ship, (i, j - k))
    go (ship, (i, j)) (East k) = (ship, (i, j + k))
    go pos (Left 0) = pos
    go (ship, (i, j)) (Left d) = go (ship, (j, - i)) (Left (d - 90))
    go pos (Right d) = go pos (Left (360 - d))
    go ((si, sj), (i, j)) (Forward k) = ((si + k * i, sj + k * j), (i, j))

    manhattan (i, j) = abs i + abs j

ex = "F10\nN3\nF7\nR90\nF11"