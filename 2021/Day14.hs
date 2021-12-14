module Day14 where

import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- readFile "Day14.txt"
  let Right (base, rules) = Parsec.parse inputP "input" input
  print $ part1 rules base
  print $ part2 rules base

part1 :: Map (Char, Char) Char -> String -> Int
part1 rules = elementDiff . (!! 10) . iterate grow
  where
    grow (a : b : rest) = a : rules ! (a, b) : grow (b : rest)
    grow end = end

    elementDiff = diff . map length . List.group . List.sort
    diff elements = maximum elements - minimum elements

part2 :: Map (Char, Char) Char -> String -> Int
part2 rules base = elementDiff $ (!! 40) $ iterate grow $ makePairs base
  where
    makePairs = Map.fromListWith (+) . flip zip (repeat 1) . (zip <*> tail)

    grow pairs = Map.foldrWithKey insertElement Map.empty pairs

    insertElement (a, b) n =
      let new = rules ! (a, b)
       in Map.insertWith (+) (a, new) n . Map.insertWith (+) (new, b) n

    baseCount = Map.singleton (head base) 1
    elementDiff = diff . Map.foldrWithKey (Map.insertWith (+) . snd) baseCount
    diff count = let freq = Map.elems count in maximum freq - minimum freq

-- Parser

inputP :: Parsec String () (String, Map (Char, Char) Char)
inputP = do
  base <- Parsec.many1 Parsec.alphaNum
  Parsec.spaces
  rules <- Map.fromList <$> Parsec.endBy rulesP Parsec.newline
  return (base, rules)

rulesP :: Parsec String () ((Char, Char), Char)
rulesP = do
  a <- Parsec.alphaNum
  b <- Parsec.alphaNum
  Parsec.string " -> "
  c <- Parsec.alphaNum
  return ((a, b), c)
