{-# LANGUAGE RecordWildCards #-}

module IntCode (IntCode, getOutput, mkIntCode) where

import Control.Monad.State.Strict
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

mkIntCode :: [Integer] -> IntCode
mkIntCode ints =  IntCode 0 prog [] 0
  where
    prog = M.fromList $ zip [0..] ints

getOutput :: IntCode -> [Integer] -> [Integer]
getOutput code inputs = reverse $ output $ execState (runProgram inputs) code

data Mode
  = Position
  | Immediate
  | Relative
  deriving Show

data OpCode
  = Add Mode Mode Mode
  | Mult Mode Mode Mode
  | Input Mode
  | Output Mode
  | JumpT Mode Mode
  | JumpF Mode Mode
  | LessThan Mode Mode Mode
  | Eq Mode Mode Mode
  | AdjustBase Mode
  | End
  deriving Show

data IntCode = IntCode
  { pos    :: Integer
  , prog   :: IntMap Integer
  , output :: [Integer]
  , base   :: Integer
  } deriving Show

update
  :: (Integer -> Integer)
     -> (IntMap Integer -> IntMap Integer)
     -> ([Integer] -> [Integer])
     -> (Integer -> Integer)
     -> IntCode
     -> IntCode
update fp fg fo fb i@(IntCode {..}) =
  i { pos = fp pos, prog = fg prog, output = fo output, base = fb base  }

getOpCode :: Integer -> OpCode
getOpCode i = case split i of
    (1, m1, m2, m3) -> Add m1 m2 m3
    (2, m1, m2, m3) -> Mult m1 m2 m3
    (3, m1, _,  _ ) -> Input m1
    (4, m1, _,  _ ) -> Output m1
    (5, m1, m2, _ ) -> JumpT m1 m2
    (6, m1, m2, _ ) -> JumpF m1 m2
    (7, m1, m2, m3) -> LessThan m1 m2 m3
    (8, m1, m2, m3) -> Eq m1 m2 m3
    (9, m1, _,  _ ) -> AdjustBase m1
    (99, _, _,  _ ) -> End
    _               -> error $ "Unknown opcode " ++ show i
  where
    split n = ( mod n 100
              , toMode $ mod (div n 100) 10
              , toMode $ mod (div n 1000) 10
              , toMode $ mod (div n 10000) 10
              )
    toMode 0 = Position
    toMode 1 = Immediate
    toMode 2 = Relative
    toMode n = error $ "Unknown parameter code " ++ show n

runProgram :: [Integer] -> State IntCode ()
runProgram inputs = do
  intcode@(IntCode i prog _ base) <- get

  let getInt :: Integer -> Integer
      getInt i = M.findWithDefault 0 (fromInteger i) prog

      replace :: Integer -> Integer -> IntMap Integer -> IntMap Integer
      replace i j = M.insert (fromInteger i) j

      readValue :: Mode -> Integer -> Integer
      readValue Position  = getInt . getInt
      readValue Immediate = getInt
      readValue Relative  = getInt . (+base) . getInt

      writeValue :: Mode -> Integer -> Integer
      writeValue Position  = getInt
      writeValue Immediate = error "Writing instruction in immediate mode"
      writeValue Relative  = (+base) . getInt

  case getOpCode (prog M.! (fromInteger i)) of
    Add m1 m2 m3 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          i3 = writeValue m3 (i+3)
      modify $ update (+4) (replace i3 (i1 + i2)) id id
      runProgram inputs

    Mult m1 m2 m3 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          i3 = writeValue m3 (i+3)
      modify $ update (+4) (replace i3 (i1 * i2)) id id
      runProgram inputs

    Input m1 -> do
      let i1 = writeValue m1 (i+1)
      modify $ update (+2) (replace i1 (head inputs)) id id
      runProgram (tail inputs)

    Output m1 -> do
      let i1 = readValue m1 (i+1)
      modify $ update (+2) id (i1:) id
      runProgram inputs

    JumpT m1 m2 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          move = if i1 /= 0 then const i2 else (+3)
      modify $ update move id id id
      runProgram inputs

    JumpF m1 m2 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          move = if i1 == 0 then const i2 else (+3)
      modify $ update move id id id
      runProgram inputs

    LessThan m1 m2 m3 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          i3 = writeValue m3 (i+3)
          val = if i1 < i2 then 1 else 0
      modify $ update (+4) (replace i3 val) id id
      runProgram inputs

    Eq m1 m2 m3 -> do
      let i1 = readValue m1 (i+1)
          i2 = readValue m2 (i+2)
          i3 = writeValue m3 (i+3)
          val = if i1 == i2 then 1 else 0
      modify $ update (+4) (replace i3 val) id id
      runProgram inputs

    AdjustBase m1 -> do
      let i1 = readValue m1 (i+1)
      modify $ update (+2) id id (+i1)
      runProgram inputs

    End -> return ()
