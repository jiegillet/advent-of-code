{-# LANGUAGE TupleSections #-}

import Data.Bits
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Register = Reg Int Int Int Int deriving (Show, Eq)
data Program = Prog Int Int Int Int deriving (Show, Eq)
data Input = In Register Program Register deriving Show
data OpCode = AddR  | AddI  | MulR  | MulI  | AndR  | AndI  | OrR  | OrI
            | SetR  | SetI | GtIR | GtRI | GtRR | EqIR | EqRI | EqRR
            deriving (Show, Eq, Enum, Ord)

getReg :: Int -> Register -> Int
getReg 0 (Reg a _ _ _ ) = a
getReg 1 (Reg _ a _ _ ) = a
getReg 2 (Reg _ _ a _ ) = a
getReg 3 (Reg _ _ _ a ) = a

putReg :: Int -> Register -> Int -> Register
putReg 0 (Reg a b c d) x = Reg x b c d
putReg 1 (Reg a b c d) x = Reg a x c d
putReg 2 (Reg a b c d) x = Reg a b x d
putReg 3 (Reg a b c d) x = Reg a b c x

apply :: OpCode -> Int -> Int -> Int -> Register -> Register
apply AddR a b c reg = putReg c reg (getReg a reg + getReg b reg)
apply AddI a b c reg = putReg c reg (getReg a reg + b)
apply MulR a b c reg = putReg c reg (getReg a reg * getReg b reg)
apply MulI a b c reg = putReg c reg (getReg a reg * b)
apply AndR a b c reg = putReg c reg (getReg a reg .&. getReg b reg)
apply AndI a b c reg = putReg c reg (getReg a reg .&. b)
apply OrR  a b c reg = putReg c reg (getReg a reg .|. getReg b reg)
apply OrI  a b c reg = putReg c reg (getReg a reg .|. b)
apply SetR a _ c reg = putReg c reg (getReg a reg)
apply SetI a _ c reg = putReg c reg a
apply GtIR a b c reg = putReg c reg $ if a > getReg b reg then 1 else 0
apply GtRI a b c reg = putReg c reg $ if getReg a reg > b then 1 else 0
apply GtRR a b c reg = putReg c reg $ if getReg a reg > getReg b reg then 1 else 0
apply EqIR a b c reg = putReg c reg $ if a == getReg b reg then 1 else 0
apply EqRI a b c reg = putReg c reg $ if getReg a reg == b then 1 else 0
apply EqRR a b c reg = putReg c reg $ if getReg a reg == getReg b reg then 1 else 0

inputSols :: Input -> [(OpCode, Int)]
inputSols (In r1 (Prog n a b c) r2) = map (,n) $ filter works [AddR .. EqRR]
  where
    works op = apply op a b c r1 == r2

getCodes :: [Input] -> Map Int OpCode
getCodes input = M.fromList $ take 16 $ getUniques opInt intOP
  where
    opInt = M.fromListWith S.union $ map (fmap S.singleton) sols
    intOP = M.fromListWith S.union $ map (fmap S.singleton . swap) sols
    sols = concatMap inputSols input
    swap (a, b) = (b, a)

    getUniques :: Map OpCode (Set Int) -> Map Int (Set OpCode) -> [(Int, OpCode)]
    getUniques opInt intOp = uniqOp ++ map swap uniqInt ++ getUniques opInt' intOp'
      where
        uniqOp  = M.toList $ M.map (S.elemAt 0) $ M.filter ((==1) . S.size) intOp
        uniqInt = M.toList $ M.map (S.elemAt 0) $ M.filter ((==1) . S.size) opInt
        ints = map snd uniqInt
        ops  = map snd uniqOp
        opInt' = M.map (flip S.difference (S.fromList ints))
                 $ foldr M.delete opInt ops
        intOp' = M.map (flip S.difference (S.fromList ops))
                 $ foldr M.delete intOp ints

runProg :: Map Int OpCode -> Program -> Register -> Register
runProg codes (Prog n a b c) = apply (codes!n) a b c

main = do
  txt <- readFile "Day16.txt"
  let (ops, prog) = parse txt
  print $ length $ filter (>2) $ map (length . inputSols) ops

  let codes = getCodes ops
  print $ foldl (flip (runProg codes)) (Reg 0 0 0 0) prog

parse :: String -> ([Input], [Program])
parse = fst . last . readP_to_S parseP

parseP :: ReadP ([Input], [Program])
parseP = (,) <$> many operationP <*> many progP

operationP :: ReadP Input
operationP =
  In <$> (string "Before: " *> regP <* skipSpaces)
     <*> progP
     <*> (string "After:  " *> regP <* skipSpaces)

regP :: ReadP Register
regP = do
  char '['
  [r1, r2, r3, r4] <- map read <$> sepBy1 (munch isDigit) (string ", ")
  char ']'
  return $ Reg r1 r2 r3 r4

progP :: ReadP Program
progP = do
  [a, b, c, d] <- map read <$> sepBy1 (munch isDigit) (char ' ')
  char '\n'
  return $ Prog a b c d
