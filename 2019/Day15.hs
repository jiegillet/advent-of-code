-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Day13 where

import           Data.Map (Map)
import qualified Data.Map as Map
import           IntCode  (IntCode, getOutput, mkIntCode)
import Control.Monad.State.Strict

data Direction
  = North
  | South
  | West
  | East
  | Stop
  deriving (Show, Eq, Enum)

data Tile
  = Wall
  | Open
  | Oxygen
  deriving (Show, Enum)

type Position = (Integer, Integer)

data Info
  = Info
    { pos :: Position
    , area :: Map Position Tile
    , path :: [(Position, [Direction])]
    , lastDir :: Direction
    }

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day15.txt"
  let intCode = mkIntCode ints
  print $ part1 intCode
  -- print $ part2 ints
  -- mapM_ print $ part2 ints
  -- putStrLn $ part2 ints
  -- mapM_ putStrLn $ part2 ints

-- part1 :: IntCode -> Int
part1 code = (out, inp, nextInputs) -- length $ path finalState
  where
    out = getOutput code inp
    inp = 1 : nextInputs
    nextInputs = [5] -- map (const 5) out
    -- (nextInputs, finalState) = runState (mapM nextMove output) initInfo

    -- initInfo = Info (0,0) (Map.singleton (0,0) Open) [((0,0), otherDirs North)] North

part3 code = out
  where
    initState = ()
    nextInputs = nextMove out ()
    input = 1 : (map fst nextInputs)
    out = getOutput code input

    nextMove [] _ = []
    nextMove (c: rest) () = (7, ()) : nextMove rest ()


-- nextMove :: Tile -> State Info Direction
-- nextMove Oxygen = return Stop
-- nextMove _ = return Stop
-- nextMove s@(Info {..}) (Wall: rest) =
--   case path of
--     [] -> [Stop]
--     ((_, []): path') ->
--       let s' = s { lastDir = dir, path = path', area = area' }
--       in  dir : nextMove s' rest
--     ((p, (d:ds)): path') ->
--       let s' = s { lastDir = d, path = ((p, ds): path'), area = area' }
--       in  d : nextMove s' rest
--   where
--     dir = opposite lastDir
--     pos' = positionAt lastDir pos
--     area' = Map.insert pos' Wall

otherDirs :: Direction -> [Direction]
otherDirs d = filter (/=d) [North, South, West, East]

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

positionAt :: Direction -> Position -> Position
positionAt North (x, y) = (x, y + 1)
positionAt South (x, y) = (x, y - 1)
positionAt West (x, y) = (x - 1, y + 1)
positionAt East (x, y) = (x + 1, y + 1)

parseOutput :: Integer -> Tile
parseOutput = toEnum . fromInteger

parseInput :: Direction -> Integer
parseInput = (+1) . fromIntegral . fromEnum
