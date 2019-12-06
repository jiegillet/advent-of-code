import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Function (on)

main = do
  txt <- readFile "Day10.txt"
  let pts = map read $ lines txt :: [Point]
      (t, msg) = converge pts
  putStr msg
  print t

data Point = Point { x :: Int, y :: Int, vx :: Int, vy :: Int } deriving Show

valP :: ReadP Int
valP = do
  skipSpaces
  sgn <- option 1 ((-1) <$ char '-')
  x <- read <$> munch1 isDigit
  return $ sgn * x

instance Read Point where
  readsPrec _ = readP_to_S pointP

pointP :: ReadP Point
pointP = do
  string "position=<"
  x <- valP
  char ','
  y <- valP
  string "> velocity=<"
  vx <- valP
  char ','
  vy <- valP
  char '>'
  skipSpaces
  return $ Point x y vx vy

--converge :: [Point] -> Int
converge pts@(Point x1 y1 vx1 vy1: Point x2 y2 vx2 vy2:rest)
  = (minT, printPoints $ evol minT)
  where
    minT =  minimumBy (compare `on` spread) [t-100..t+100]
    t = div (y2 - y1) (vy1 - vy2) -- div (x2 - x1) (vx1 - vx2)
    evol t = map (\(Point x y vx vy) -> Point (x+t*vx) (y+t*vy) vx vy) pts
    spread t = dx * dy
      where
        dx = let xspan = map x (evol t) in maximum xspan - minimum xspan
        dy = let yspan = map y (evol t) in maximum yspan - minimum yspan


printPoints :: [Point] -> String
printPoints pts = unlines [ [ if any (isPoint x y) pts then '#' else '.' | x <- [x0..xN]] | y <- [y0..yN]]
  where
    x0 = minimum $ map x pts
    xN = maximum $ map x pts
    y0 = minimum $ map y pts
    yN = maximum $ map y pts
    isPoint x y (Point a b _ _) = x==a && y==b
