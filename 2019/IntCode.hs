{-# LANGUAGE RecordWildCards #-}

module IntCode (IntCode, getOutput, mkIntCode) where

import Data.Array
import Control.Monad.State.Strict

mkIntCode :: [Int] -> IntCode
mkIntCode ints =  IntCode 0 prog []
  where
    prog = listArray (0, length ints - 1) ints

getOutput :: IntCode -> [Int] -> [Int]
getOutput code inputs = reverse $ output $ execState (runProgram inputs) code

data Mode = Position | Immediate deriving Show

data OpCode
  = Add Mode Mode
  | Mult Mode Mode
  | Input
  | Output Mode
  | JumpT Mode Mode
  | JumpF Mode Mode
  | LessThan Mode Mode
  | Eq Mode Mode
  | End
  deriving Show

data IntCode = IntCode
  { pos :: Int
  , prog :: Array Int Int
  , output :: [Int]
  } deriving Show

update
  :: (Int -> Int)
     -> (Array Int Int -> Array Int Int)
     -> ([Int] -> [Int])
     -> IntCode
     -> IntCode
update fp fg fo i@(IntCode {..}) =
  i { pos = fp pos, prog = fg prog, output = fo output  }

getOpCode :: Int -> OpCode
getOpCode i = case split i of
    (1, m1, m2) -> Add m1 m2
    (2, m1, m2) -> Mult m1 m2
    (3, _, _)   -> Input
    (4, m1, _)  -> Output m1
    (5, m1, m2) -> JumpT m1 m2
    (6, m1, m2) -> JumpF m1 m2
    (7, m1, m2) -> LessThan m1 m2
    (8, m1, m2) -> Eq m1 m2
    (99, _, _)  -> End
    _ -> error $ "Unknown opcode " ++ show i
  where
    split n = ( mod n 100
              , toMode $ mod (div n 100) 10
              , toMode $ mod (div n 1000) 10
              )
    toMode 0 = Position
    toMode 1 = Immediate
    toMode n = error ("Unknown parameter code " ++ show n)

getValue :: Mode -> Array Int Int -> Int -> Int
getValue Position prog = (prog!) . (prog!)
getValue Immediate prog = (prog!)

runProgram :: [Int] -> State IntCode ()
runProgram inputs = do
  IntCode i prog out <- get

  case getOpCode (prog!i) of
    Add m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          i3 = getValue Immediate prog (i+3)
      modify $ update (+4) (// [(i3, i1 + i2)]) id
      runProgram inputs

    Mult m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          i3 = getValue Immediate prog (i+3)
      modify $ update (+4) (// [(i3, i1 * i2)]) id
      runProgram inputs

    Input -> do
      let i1 = getValue Immediate prog (i+1)
      modify $ update (+2) (// [(i1, head inputs)]) id
      runProgram (tail inputs)

    Output m1 -> do
      let i1 = getValue m1 prog (i+1)
      modify $ update (+2) id (i1:)
      runProgram inputs

    JumpT m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          move = if i1 /= 0 then const i2 else (+3)
      modify $ update move id id
      runProgram inputs

    JumpF m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          move = if i1 == 0 then const i2 else (+3)
      modify $ update move id id
      runProgram inputs

    LessThan m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          i3 = getValue Immediate prog (i+3)
          val = if i1 < i2 then 1 else 0
      modify $ update (+4) (// [(i3, val)]) id
      runProgram inputs

    Eq m1 m2 -> do
      let i1 = getValue m1 prog (i+1)
          i2 = getValue m2 prog (i+2)
          i3 = getValue Immediate prog (i+3)
          val = if i1 == i2 then 1 else 0
      modify $ update (+4) (// [(i3, val)]) id
      runProgram inputs

    End -> return ()
