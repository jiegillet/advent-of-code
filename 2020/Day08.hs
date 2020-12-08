{-# LANGUAGE TupleSections #-}

module Day08 where

import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec

main :: IO ()
main = do
  program <- readFile "Day08.txt"
  let Right operations = parse programP "program" program
  --   let Right operations = parse programP "program" ex
  print $ part1 operations
  print $ part2 operations

part1 :: [Op] -> Integer
part1 (op : operations) = noLoop Set.empty $ iterate (uncurry step) $ (0, (fromList op operations))
  where
    noLoop history ((acc, zip) : rest)
      | Set.member (position zip) history = acc
      | otherwise = noLoop (Set.insert (position zip) history) rest

part2 :: [Op] -> [Maybe Integer]
part2 (op : operations) = filter (/= Nothing) $ map (terminates Set.empty) programs
  where
    terminates :: Set.Set Integer -> [(Integer, ZipList Op)] -> Maybe Integer
    terminates history ((acc, zip) : rest)
      | Set.member (position zip) history = Nothing
      | rightMost zip = Just $ fst $ step acc zip
      | otherwise = terminates (Set.insert (position zip) history) rest

    program = fromList op operations
    noOpAndJmpPos = map fst $ filter (isNoOpOrJmp . snd) $ zip [0 ..] (op : operations)
    zipVariations = map flipNoOpAndJmpPos noOpAndJmpPos
    programs = map (iterate (uncurry step) . (0,)) zipVariations

    flipNoOpAndJmpPos i = shift (- i) $ mapCurrent flip $ shift i $ program

    flip (NoOp k) = Jmp k
    flip (Jmp k) = NoOp k

    isNoOpOrJmp (NoOp _) = True
    isNoOpOrJmp (Jmp _) = True
    isNoOpOrJmp _ = False

step :: Integer -> (ZipList Op) -> (Integer, ZipList Op)
step acc zip@(ZipList _ _ (NoOp _) _) = (acc, right zip)
step acc zip@(ZipList _ _ (Acc k) _) = (acc + k, right zip)
step acc zip@(ZipList _ _ (Jmp j) _) = (acc, shift j zip)

data ZipList a = ZipList Integer [a] a [a]
  deriving (Show)

fromList :: a -> [a] -> ZipList a
fromList a as = ZipList 0 [] a as

current :: ZipList a -> a
current (ZipList _ _ a _) = a

mapCurrent :: (a -> a) -> ZipList a -> ZipList a
mapCurrent f (ZipList i l a r) = ZipList i l (f a) r

right :: ZipList a -> ZipList a
right (ZipList i l a []) = ZipList i l a []
right (ZipList i l a (r : rs)) = ZipList (i + 1) (a : l) r rs

left :: ZipList a -> ZipList a
left (ZipList i [] a r) = ZipList i [] a r
left (ZipList i (l : ls) a r) = ZipList (i - 1) ls l (a : r)

shift :: Integer -> ZipList a -> ZipList a
shift 0 zip = zip
shift n zip
  | n < 0 = shift (n + 1) (left zip)
  | n > 0 = shift (n - 1) (right zip)

position :: ZipList a -> Integer
position (ZipList i _ _ _) = i

leftMost :: ZipList a -> Bool
leftMost (ZipList _ [] _ _) = True
leftMost _ = False

rightMost :: ZipList a -> Bool
rightMost (ZipList _ _ _ []) = True
rightMost _ = False

data Op
  = Acc Integer
  | Jmp Integer
  | NoOp Integer
  deriving (Show)

programP :: Parsec String () [Op]
programP = sepEndBy1 operationP newline

operationP :: Parsec String () Op
operationP = accumulateP <|> jumpP <|> noOpP

accumulateP, jumpP, noOpP :: Parsec String () Op
accumulateP = Acc <$> (string "acc " >> integerP)
jumpP = Jmp <$> (string "jmp " >> integerP)
noOpP = NoOp <$> (string "nop " >> integerP)

integerP :: Parsec String () Integer
integerP = do
  sgn <- oneOf "+-"
  int <- read <$> many digit
  case sgn of
    '+' -> return int
    '-' -> return $ - int

ex =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]