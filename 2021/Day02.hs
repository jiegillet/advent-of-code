module Day02 where

import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  directions <- readFile "Day02.txt"
  let Right dirs = Parsec.parse directionsP "Directions" directions
  print $ part1 dirs
  print $ part2 dirs

type Aim = Int

data Position = Position Int Int

data Direction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

part1 :: [Direction] -> Int
part1 = mult . foldl move (Position 0 0)

move :: Position -> Direction -> Position
move (Position hor ver) (Forward f) = Position (hor + f) ver
move (Position hor ver) (Down d) = Position hor (ver + d)
move (Position hor ver) (Up u) = Position hor (ver - u)

part2 :: [Direction] -> Int
part2 = mult . snd . foldl moveWithAim (0, Position 0 0)

moveWithAim :: (Aim, Position) -> Direction -> (Aim, Position)
moveWithAim (aim, Position hor ver) (Forward f) = (aim, Position (hor + f) (ver + aim * f))
moveWithAim (aim, pos) (Down d) = (aim + d, pos)
moveWithAim (aim, pos) (Up u) = (aim - u, pos)

mult :: Position -> Int
mult (Position hor ver) = hor * ver

-- Parser

directionsP :: Parsec String () [Direction]
directionsP = Parsec.sepEndBy directionP Parsec.newline

directionP :: Parsec String () Direction
directionP = do
  direction <-
    Parsec.choice
      [ Forward <$ Parsec.string "forward",
        Down <$ Parsec.string "down",
        Up <$ Parsec.string "up"
      ]
  Parsec.spaces
  steps <- read <$> Parsec.many Parsec.digit
  return $ direction steps