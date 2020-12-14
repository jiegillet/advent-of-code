module Main where

import Data.Bits
import qualified Data.IntMap as Map
import Text.Parsec
  ( Parsec,
    digit,
    many,
    newline,
    oneOf,
    parse,
    sepEndBy1,
    string,
    try,
    (<|>),
  )

main :: IO ()
main = do
  input <- readFile "Day14.txt"
  let Right inputs = parse inputsP "input" input
  print $ part1 inputs
  print $ part2 inputs

part1 :: [Input] -> Int
part1 = sum . Map.elems . snd . foldl go (Mask 0 0, Map.empty)
  where
    go (_, mem) mask'@(Mask _ _) = (mask', mem)
    go (mask, memory) (Memory address value) = (mask, Map.insert address (masked mask value) memory)

    masked (Mask to1 to0) = (to0 .&.) . (to1 .|.)

part2 :: [Input] -> Int
part2 = sum . Map.elems . snd . foldl go (Mask 0 0, Map.empty)
  where
    go (_, mem) mask@(Mask _ _) = (mask, mem)
    go (mask, memory) (Memory address value) = (mask, foldr (\k m -> Map.insert k value m) memory (masked mask address))

    masked (Mask to1 to0) address = map (xor (to1 .|. address)) masks
      where
        x = xor to1 to0
        masks = map (foldr1 (\b n -> 2 * n + b)) $ sequence $ map (\b -> if testBit x b then [1, 0] else [0]) [0 .. 35]

data Input
  = Mask Int Int
  | Memory Int Int
  deriving (Show)

inputsP :: Parsec String () [Input]
inputsP = sepEndBy1 inputP newline

inputP :: Parsec String () Input
inputP = try maskP <|> memoryP

maskP :: Parsec String () Input
maskP = do
  string "mask = "
  mask <- many (oneOf "01X")
  let to1 = foldl (\n c -> if c == '1' then 2 * n + 1 else 2 * n) 0 mask
      to0 = foldl (\n c -> if c == '0' then 2 * n else 2 * n + 1) 0 mask
  return $ Mask to1 to0

memoryP :: Parsec String () Input
memoryP = do
  string "mem["
  mem <- read <$> many digit
  string "] = "
  val <- read <$> many digit
  return $
    Memory
      mem
      val
