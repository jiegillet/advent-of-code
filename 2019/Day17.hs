module Day07 where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           IntCode   (IntCode, getOutput, mkIntCode)
import Data.Char (chr)
import Data.List (zipWith6)

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
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day17.txt"
  let intCode = mkIntCode ints
  putStrLn $ map (chr . fromInteger) $ getOutput intCode []
  print $ part1 intCode
  -- putStrLn $ part2 intCode

-- part1 :: IntCode -> Integer
part1 code = param
  where
    allSharp n '#' '#' '#' '#' '#'
      | j > 0 && j < side - 1 = j * (div n side + 1)
      | otherwise = 0
      where
        j = mod n side
    allSharp _ _ _ _ _ _ = 0
    side = length (takeWhile (/='\n') output) + 1
    output = map (chr . fromInteger) $ getOutput code []
    param = sum $ zipWith6 allSharp [0..]
                                     output
                                     (drop (side-1) output)
                                     (drop side output)
                                     (drop (side+1) output)
                                     (drop (2*side) output)

test = "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^.."

-- ..........#########..........................
-- ..........#.......#..........................
-- ..........#.......#..........................
-- ..........#.......#..........................
-- ..........#.......#..........................
-- ..........#.......#..........................
-- ..........#####...#...........#######.#######
-- ..............#...#...........#.....#.#.....#
-- ..........####O###O##.........#.....#.#.....# (8, 14) (8, 18)
-- ..........#...#...#.#.........#.....#.#.....#
-- ..........#...#...#.#.....####O#####O##.....# (10, 30) (10,36)
-- ..........#...#...#.#.....#...#.....#.......#
-- ..........#...#...##O#####O####.....#.......# (12, 20) (12, 26)
-- ..........#...#.....#.....#.........#.......#
-- ..........#...#.....#.....#.....####O######## (14, 36)
-- ..............#.....#.....#.....#...#........
-- ..............#.....#.....######O####........ (16, 32)
-- ..............#.....#...........#............
-- ..............#######...........#............
-- ................................#............
-- ................................#............
-- ................................#............
-- ............................####O##.......... (22, 32)
-- ............................#...#.#..........
-- ............................#...#.#..........
-- ............................#...#.#..........
-- ............................#...##O####...... (26, 34)
-- ............................#.....#...#......
-- ..................##########O##...#...#...... (28, 28)
-- ..................#.........#.#...#...#......
-- ..................#.........#.#...#...#......
-- ..................#.........#.#...#...#......
-- ..................#.........##O###O####...... (32, 30) (32, 34)
-- ..................#...........#...#..........
-- ......#############...........#...#####......
-- ......#.......................#.......#......
-- ......#.......................#.......#......
-- ......#.......................#.......#......
-- ^######.......................#.......#......
-- ..............................#.......#......
-- ..............................#########......
