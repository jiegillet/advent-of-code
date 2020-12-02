-- {-# OPTIONS_GHC -Wall #-}
module Day13 where

import Data.List (scanl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           IntCode  (IntCode, getOutput, mkIntCode)

type Position = (Integer, Integer)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  | Score Integer
  deriving Eq

instance Show Tile where
  show Empty     = " "
  show Wall      = "X"
  show Block     = "="
  show Paddle    = "_"
  show Ball      = "o"
  show (Score n) = "Score: " ++ show n

data Joystick
  = Neutral
  | Lft
  | Rght
  deriving Show

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day13.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode
  print $ part2 ints
  -- mapM_ print $ part2 ints
  -- putStrLn $ part2 ints
  -- mapM_ putStrLn $ part2 ints

part1 :: IntCode -> Int
part1 code = numBlocks $ foldr (uncurry Map.insert) Map.empty output
  where
    output = map parseOutput $ chunks 3 $ getOutput code []

    numBlocks = length . Map.filter (==Block)

parseOutput :: [Integer] -> (Position, Tile)
parseOutput [-1, 0, n] = ((-1, 0), Score n)
parseOutput [x, y, 0]  = ((x, y), Empty)
parseOutput [x, y, 1]  = ((x, y), Wall)
parseOutput [x, y, 2]  = ((x, y), Block)
parseOutput [x, y, 3]  = ((x, y), Paddle)
parseOutput [x, y, 4]  = ((x, y), Ball)
parseOutput _          = error "Unexpected output"

-- part2 :: [Integer] -> Int
-- part2 :: [Integer] -> [(Position, Tile)]
-- part2 [] = []
part2 (_: ints) = -1 : nextMoves initPaddle (drop size output) -- map render $ drop size states -- $ drop size $ output
  where
    output = map parseOutput $ chunks 3 $ getOutput (mkIntCode (2: ints)) input
    states = scanl' (flip $ uncurry Map.insert) Map.empty output

    size = 43 * 21 + 1
    initPaddle = fst $ fst $ head $ filter ((==Paddle) . snd) $ output

    input =  0 : nextMoves initPaddle (drop size output)
    nextMoves _ [] = []
    nextMoves xp (((x, _), Ball):rest) = signum (x - xp) : nextMoves xp rest
    nextMoves xp (_:rest) = nextMoves xp rest

    -- nextMoves _ [] = []
    -- nextMoves (xp, xb0) (xb : rest) =
    --   let mv = if xp /= xb0 then 0 else signum (xb - xb0)
    --   in mv: nextMoves (xp + mv, xb) rest


render :: Map Position Tile -> String
render g = unlines (map concat pixels ++ [score])
  where
    (xmin, xmax) = (0, 42)
    (ymin, ymax) = (0, 20)
    pixels = [[ get (x, y) | x <- [xmin..xmax]] | y <- [ymin .. ymax]]
    get p = show $ Map.findWithDefault Empty p g
    score = show $ Map.findWithDefault (Score (-1)) (-1, 0) g

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = let (a, b) = splitAt n l in a : chunks n b
