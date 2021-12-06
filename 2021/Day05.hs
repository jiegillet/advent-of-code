module Day05 where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- readFile "Day05.txt"
  let Right vents = Parsec.parse ventsP "vents" input
  print $ part1 vents
  print $ part2 vents

type Aim = Int

data Vent = Vent (Int, Int) (Int, Int) deriving (Show)

part1 :: [Vent] -> Int
part1 = flip totalOverlap Set.empty . filter straight
  where
    straight (Vent (fromX, fromY) (toX, toY)) = fromX == toX || fromY == toY

    totalOverlap [] set = Set.size set
    totalOverlap (vent : vents) set = totalOverlap vents (foldr (overlap vent) set vents)

    overlap :: Vent -> Vent -> Set (Int, Int) -> Set (Int, Int)
    overlap vent1@(Vent (x0, y0) (x1, y1)) vent2@(Vent (x2, y2) (x3, y3)) set
      | sameHoriZontal vent1 vent2 = foldr Set.insert set [(x0, y) | y <- lineOverlap (y0, y1) (y2, y3)]
      | sameVertical vent1 vent2 = foldr Set.insert set [(x, y0) | x <- lineOverlap (x0, x1) (x2, x3)]
      | straightCross1 vent1 vent2 = Set.insert (x0, y2) set
      | straightCross2 vent1 vent2 = Set.insert (x2, y0) set
      | otherwise = set

sameHoriZontal, sameVertical, straightCross1, straightCross2 :: Vent -> Vent -> Bool
sameHoriZontal (Vent (x0, _) (x1, _)) (Vent (x2, _) (x3, _)) = x0 == x1 && x2 == x3 && x0 == x2
sameVertical (Vent (_, y0) (_, y1)) (Vent (_, y2) (_, y3)) = y0 == y1 && y2 == y3 && y0 == y2
straightCross1 (Vent (x0, y0) (x1, y1)) (Vent (x2, y2) (x3, y3)) =
  x0 == x1 && y2 == y3 && min x2 x3 <= x0 && x0 <= max x2 x3 && min y0 y1 <= y2 && y2 <= max y0 y1
straightCross2 (Vent (x0, y0) (x1, y1)) (Vent (x2, y2) (x3, y3)) =
  y0 == y1 && x2 == x3 && min y2 y3 <= y0 && y0 <= max y2 y3 && min x0 x1 <= x2 && x2 <= max x0 x1

lineOverlap :: (Enum a, Ord a) => (a, a) -> (a, a) -> [a]
lineOverlap (x0, x1) (x2, x3)
  | max x0 x1 < min x2 x3 = []
  | otherwise =
    let right = min (max x0 x1) (max x2 x3)
        left = max (min x0 x1) (min x2 x3)
     in [left .. right]

-- part2 :: [Vent] -> Int
part2 = flip totalOverlap Set.empty
  where
    totalOverlap [] set = Set.size set
    totalOverlap (vent : vents) set = totalOverlap vents (foldr (overlap vent) set vents)

    overlap :: Vent -> Vent -> Set (Int, Int) -> Set (Int, Int)
    overlap vent1 vent2 = Set.union (Set.intersection (span vent1) (span vent2))

    span :: Vent -> Set (Int, Int)
    span (Vent (x0, y0) (x1, y1))
      | x0 == x1 = Set.fromList [(x0, y) | y <- [min y0 y1 .. max y0 y1]]
      | y0 == y1 = Set.fromList [(x, y0) | x <- [min x0 x1 .. max x0 x1]]
      | otherwise =
        let dx = if x1 >= x0 then 1 else -1
            dy = if y1 > y0 then 1 else -1
         in Set.fromList [(x0 + dx * d, y0 + dy * d) | d <- [0 .. abs (x0 - x1)]]

-- Parser

ventsP :: Parsec String () [Vent]
ventsP = Parsec.sepEndBy ventP Parsec.newline

ventP :: Parsec String () Vent
ventP = do
  fromX <- read <$> Parsec.many Parsec.digit
  Parsec.char ','
  fromY <- read <$> Parsec.many Parsec.digit
  Parsec.string " -> "
  toX <- read <$> Parsec.many Parsec.digit
  Parsec.char ','
  toY <- read <$> Parsec.many Parsec.digit
  return $ Vent (fromX, fromY) (toX, toY)