module Day02 where

import Data.Set (Set)
import qualified Data.Set
import Data.Map (Map)
import qualified Data.Map
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = do
  [wire1, wire2] <- lines <$> readFile "Day03.txt"
  let Right dir1 = parse dirsP "First wire" wire1
      Right dir2 = parse dirsP "Second wire" wire2
  print $ part1 dir1 dir2
  print $ part2 dir1 dir2

data Dir = Up Integer | Down Integer | Rght Integer | Lft Integer deriving Show

part1 :: [Dir] -> [Dir] -> Integer
part1 d1 d2 = Data.Set.findMin $
    Data.Set.map (\(i, j) -> abs i + abs j) $
    Data.Set.intersection
      (Data.Set.fromList $ path d1)
      (Data.Set.fromList $ path d2)

part2 :: [Dir] -> [Dir] -> Integer
part2 d1 d2 = minimum $
    Data.Map.elems $
    Data.Map.intersectionWith (+)
      (Data.Map.fromListWith min $ zip (path d1) [1..])
      (Data.Map.fromListWith min $ zip (path d2) [1..])

path :: [Dir] -> [(Integer, Integer)]
path = go (0,0)
  where
    go _ [] = []
    go (i, j) (Up d: rest)   = [(i, j + k) | k <- [1..d]] ++ go (i, j + d) rest
    go (i, j) (Down d: rest) = [(i, j - k) | k <- [1..d]] ++ go (i, j - d) rest
    go (i, j) (Lft d: rest)  = [(i - k, j) | k <- [1..d]] ++ go (i - d, j) rest
    go (i, j) (Rght d: rest) = [(i + k, j) | k <- [1..d]] ++ go (i + d, j) rest

dirsP :: Parsec String () [Dir]
dirsP = sepBy dirP (char ',')

dirP :: Parsec String () Dir
dirP = do
  d <- oneOf "UDLR"
  i <- read <$> many digit
  return $ case d of
    'U' -> Up i
    'D' -> Down i
    'L' -> Lft i
    'R' -> Rght i
