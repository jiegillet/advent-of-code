module Day01 where

import qualified Data.Either as Either
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- readFile "Day13.txt"
  let Right (coord, folds) = Parsec.parse inputP "input" input
  print $ part1 coord folds
  putStrLn $ part2 coord folds

data Fold = FoldX Int | FoldY Int deriving (Show)

part1 :: [(Int, Int)] -> [Fold] -> Int
part1 coord = Set.size . flip fold (Set.fromList coord) . head

fold :: Fold -> Set (Int, Int) -> Set (Int, Int)
fold (FoldX fold) = Set.map (\(x, y) -> if x > fold then (2 * fold - x, y) else (x, y))
fold (FoldY fold) = Set.map (\(x, y) -> if y > fold then (x, 2 * fold - y) else (x, y))

part2 :: [(Int, Int)] -> [Fold] -> String
part2 coord = draw . foldl (flip fold) (Set.fromList coord)
  where
    draw numbers =
      unlines
        [ [if Set.member (x, y) numbers then '#' else ' ' | x <- [xMin .. xMax]]
          | y <- [yMin .. yMax]
        ]
      where
        Just ((xMin, _), _) = Set.minView numbers
        Just ((xMax, _), _) = Set.maxView numbers
        Just (yMin, _) = Set.minView $ Set.map snd numbers
        Just (yMax, _) = Set.maxView $ Set.map snd numbers

-- Parser

inputP :: Parsec String () ([(Int, Int)], [Fold])
inputP =
  Either.partitionEithers
    <$> Parsec.sepBy
      (Parsec.choice [Left <$> coordP, Right <$> foldP])
      Parsec.spaces

coordP :: Parsec String () (Int, Int)
coordP = do
  x <- read <$> Parsec.many Parsec.digit
  Parsec.char ','
  y <- read <$> Parsec.many Parsec.digit
  return (x, y)

foldP :: Parsec String () Fold
foldP = do
  Parsec.string "fold along "
  fold <-
    Parsec.choice
      [ FoldX <$ Parsec.char 'x',
        FoldY <$ Parsec.char 'y'
      ]
  Parsec.char '='
  x <- read <$> Parsec.many Parsec.digit
  return $ fold x