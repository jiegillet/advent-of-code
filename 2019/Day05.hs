module Day05 where

import Data.Array
import Control.Monad.State.Strict

main :: IO ()
main = do
  ints <- read . (\i ->  "[" ++ i ++ "]") <$> readFile "Day05.txt"
  let prog = listArray (0, length ints - 1) ints
      intCode = IntCode 0 prog Position []
  print $ part1 [1] intCode
  print $ part2 [5] intCode

data OpCode
  = Add
  | Mult
  | Input
  | Output
  | JumpT
  | JumpF
  | LessThan
  | Eq
  | End deriving Show

data IntCode = IntCode
  { pos :: Int
  , prog :: Array Int Int
  , mode :: Mode
  , output :: [Int]
  } deriving Show

data Mode = Position | Immediate deriving Show

updateProg :: (Array Int Int -> Array Int Int) -> IntCode -> IntCode
updateProg f i = i { prog = f (prog i) }

updatePos :: (Int -> Int) -> IntCode -> IntCode
updatePos f i = i { pos = f (pos i) }

updateOutput :: ([Int] -> [Int]) -> IntCode -> IntCode
updateOutput f i = i { output = f (output i) }


part1 :: [Int] -> IntCode -> [Int]
part1 inputs = output . execState (runProgram inputs)

part2 = part1

getMode :: Int -> (OpCode, [Mode])
getMode 99 = (End, [])
getMode n = case split n of
    (1, m1, m2) -> (Add, [m1, m2])
    (2, m1, m2) -> (Mult, [m1, m2])
    (3, _, _)   -> (Input, [])
    (4, m1, _)  -> (Output, [m1])
    (5, m1, m2) -> (JumpT, [m1, m2])
    (6, m1, m2) -> (JumpF, [m1, m2])
    (7, m1, m2) -> (LessThan, [m1, m2])
    (8, m1, m2) -> (Eq, [m1, m2])
    _ -> error $ "bad code" ++ show n
  where
    split n = ( mod n 100
              , toMode $ mod (div n 100) 10
              , toMode $ mod (div n 1000) 10
              )
    toMode 0 = Position
    toMode 1 = Immediate
    toMode n = error "wrong parameter code " ++ show n

getValue :: Mode -> Array Int Int -> Int -> Int
getValue Position prog i = prog!i
getValue Immediate _ i = i

runProgram :: [Int] -> State IntCode ()
runProgram inputs = do
  IntCode i prog mode out <- get
  case getMode (prog!i) of
    (End, []) -> return ()

    (Add, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      modify $ updateProg (// [(prog!(i+3), prog!i1 + prog!i2)])
      modify $ updatePos (+4)
      runProgram inputs

    (Mult, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      modify $ updateProg (// [(prog!(i+3), prog!i1 * prog!i2)])
      modify $ updatePos (+4)
      runProgram inputs

    (Input, _) -> do
      modify $ updateProg (// [(prog!(i+1), head inputs)])
      modify $ updatePos (+2)
      runProgram (tail inputs)

    (Output, [m1]) -> do
      let i1 = getValue m1 prog (i+1)
      modify $ updateOutput (prog!i1:)
      modify $ updatePos (+2)
      runProgram inputs

    (JumpT, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      if prog!i1 /= 0
        then modify $ updatePos (const (prog!i2))
        else modify $ updatePos (+3)
      runProgram inputs

    (JumpF, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      if prog!i1 == 0
        then modify $ updatePos (const (prog!i2))
        else modify $ updatePos (+3)
      runProgram inputs

    (LessThan, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      if prog!i1 < prog!i2
        then modify $ updateProg (// [(prog!(i+3), 1)])
        else modify $ updateProg (// [(prog!(i+3), 0)])
      modify $ updatePos (+4)
      runProgram inputs

    (Eq, [m1, m2]) -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
      if prog!i1 == prog!i2
        then modify $ updateProg (// [(prog!(i+3), 1)])
        else modify $ updateProg (// [(prog!(i+3), 0)])
      modify $ updatePos (+4)
      runProgram inputs

    _ -> error( "Unknown opcode" ++ show (prog!i))
