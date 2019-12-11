module Day07 where

import           Data.List (permutations)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import           IntCode   (IntCode, getOutput, mkIntCode)

type Position = (Integer, Integer)

data Dir
  = Up
  | Down
  | Lft
  | Rght
  deriving Show

data State = State
    { direction :: Dir
    , position  :: Position
    , colors    :: Map Position Integer
    }

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day11.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode
  putStrLn $ part2 intCode

part1 :: IntCode -> Int
part1 code = Map.size $ colors $ snd $ last nextInputs
  where
    initState = State Up (0, 0) Map.empty
    nextInputs = nextMove out initState
    input = 0 : (map fst nextInputs)
    out = getOutput code input

part2 :: IntCode -> String
part2 code = unlines pixels
  where
    initState = State Up (0, 0) Map.empty
    nextInputs = nextMove out initState
    input = 1 : (map fst nextInputs)
    out = getOutput code input

    paint = colors $ snd $ last nextInputs
    xmin = fst $ fst $ Map.findMin paint
    xmax = fst $ fst $ Map.findMax paint
    ymin = minimum $ map snd $ Map.keys paint
    ymax = maximum $ map snd $ Map.keys paint

    pixels = [[ char (x, y) | x <- [xmin..xmax]] | y <- [ymax, ymax-1..ymin]]
    char pos = if (Map.lookup pos paint) == Just 1
                 then 'X'
                 else ' '

nextMove :: [Integer] -> State -> [(Integer, State)]
nextMove [] _ = []
nextMove (color: turn: rest) (State dir pos colors) =
  let colors' = Map.insert pos color colors
      dir' = makeTurn turn dir
      pos' = move dir' pos
      state' = State dir' pos' colors'
      nextColor = Map.findWithDefault 0 pos' colors
  in (nextColor, state') : nextMove rest state'

makeTurn :: Integer -> Dir -> Dir
makeTurn 1 Up   = Rght
makeTurn 1 Rght = Down
makeTurn 1 Down = Lft
makeTurn 1 Lft  = Up
makeTurn 0 Up   = Lft
makeTurn 0 Rght = Up
makeTurn 0 Down = Rght
makeTurn 0 Lft  = Down
makeTurn _ _    = error "Wrong turn code"

move :: Dir -> Position -> Position
move Up (x, y)   = (x, y + 1)
move Down (x, y) = (x, y - 1)
move Rght (x, y) = (x + 1, y)
move Lft (x, y)  = (x - 1, y)
