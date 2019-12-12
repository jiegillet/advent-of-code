module Day12 where

import qualified Data.Set         as Set
import           Text.Parsec
import           Text.Parsec.Char

main :: IO ()
main = do
  pos <- readFile "Day12.txt"
  let Right positions = parse positionsP "Initial Positions" pos
  print $ part1 positions
  print $ part2 positions

data Moon = Moon Position Velocity deriving Show
data Position = P Int Int Int deriving Show
data Velocity = V Int Int Int deriving Show

part1 :: [Moon] -> Int
part1 = sum . map energy . (!!1000) . iterate step
  where
    step = map velocity . gravity

part2 :: [Moon] -> Int
part2 = findCycles . iterate step
  where
    step = map velocity . gravity

    findCycles :: [[Moon]] -> Int
    findCycles m = xyzC
      where
        xyzC = div (xyC * zC) (gcd xyC zC)
        xyC = div (xC * yC) (gcd xC yC)
        xC = cycleLength $ map (map (\(Moon (P x _ _) (V vx _ _)) -> (x, vx))) m
        yC = cycleLength $ map (map (\(Moon (P _ y _) (V _ vy _)) -> (y, vy))) m
        zC = cycleLength $ map (map (\(Moon (P _ _ z) (V _ _ vz)) -> (z, vz))) m

cycleLength :: (Ord a) => [a] -> Int
cycleLength = go Set.empty
  where
    go set (a:as)
      | Set.member a set = Set.size set
      | otherwise = go (Set.insert a set) as

gravity :: [Moon] -> [Moon]
gravity moons = map grav moons
  where
    grav moon = foldr go moon moons

    go (Moon (P x' y' z') _) (Moon (P x y z) (V vx vy vz))
        = Moon (P x y z) (V (vx+signum (x'-x))
                            (vy+signum (y'-y))
                            (vz+signum (z'-z)) )

velocity :: Moon -> Moon
velocity (Moon (P x y z) v@(V vx vy vz)) = Moon (P (x+vx) (y+vy) (z+vz)) v

energy :: Moon -> Int
energy (Moon (P x y z) (V vx vy vz)) = pot * kin
  where
    pot = abs x + abs y + abs z
    kin = abs vx + abs vy + abs vz

positionsP :: Parsec String () [Moon]
positionsP = sepEndBy positionP (char '\n')

positionP :: Parsec String () Moon
positionP = do
  string "<x="
  xsign <- option '0' (char '-')
  x <- read . (xsign:) <$> many digit
  string ", y="
  ysign <- option '0' (char '-')
  y <- read . (ysign:) <$> many digit
  string ", z="
  zsign <- option '0' (char '-')
  z <- read . (zsign:) <$> many digit
  string ">"
  return $ Moon (P x y z) (V 0 0 0)
