module Day13 where

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
  -- mapM_ putStrLn $ part2 ints
  -- print $ part2 ints

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
parseOutput _          = error "Illegal output"

-- part2 :: [Integer] -> Int
-- part2 (_: ints) = undefined -- map render $ parseOutput Map.empty output
--   where
--     output = map parseOutput $ chunks 3 $ getOutput (mkIntCode (2: ints)) input
--
--     input = filter ((==Block) . snd)
--
--
--     beam = parseOutput output
--
--
--     parseOutput g (x:y:4:rest)    = let g' = update (x, y) 4 g in  g' : parseOutput g' rest
--     parseOutput g (x:y:tile:rest) = parseOutput (update (x, y) tile g) rest
--     input =  filter ((==4) . last) $ chuncks 2 output

render :: Map Position Tile -> String
render g = unlines (map concat pixels ++ [score])
  where
    (xmin, xmax) = (0, 42)
    (ymin, ymax) = (0, 20)
    pixels = [[ get (x, y) | x <- [xmin..xmax]] | y <- [ymin .. ymax]]
    get p = show $ Map.findWithDefault Empty p g
    score = show $ Map.findWithDefault (Score 0) (-1, 0) g

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = let (a, b) = splitAt n l in a : chunks n b
