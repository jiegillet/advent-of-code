module Day02 where

import Data.Array
import Control.Monad.State.Strict

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day02.txt"
  let input = listArray (0, length ints - 1) ints
  print $ part1 12 2 input
  print $ part2 input


part1 :: Int -> Int -> Array Int Int -> Int
part1 noun verb = (!0) . execState (runProgram 0) . (// [(1, noun), (2, verb)])

part2 :: Array Int Int -> Int
part2 input = head [100 * noun + verb
                             | noun <- [0..99],
                               verb <- [0..99],
                               part1 noun verb input == 19690720]


runProgram :: Int -> State (Array Int Int) ()
runProgram i = do
  prog <- get
  case prog!i of
    99 -> return ()
    1 -> do
      modify (// [(prog!(i+3), prog!(prog!(i+1)) + prog!(prog!(i+2)))])
      runProgram (i+4)

    2 -> do
      modify (// [(prog!(i+3), prog!(prog!(i+1)) * prog!(prog!(i+2)))])
      runProgram (i+4)

    n -> error( "Unknown opcode" ++ show n)
