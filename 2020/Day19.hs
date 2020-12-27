module Main where

import Data.Either (isRight)
import Data.List (inits)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "Day19.txt"
  let Right (rules, base, samples) = parse inputP "input" input
  -- let Right (rules, base, samples) = parse inputP "input" ex2
  print $ part1 rules base samples
  print $ part2 rules base samples

part1 :: Map Int [[Int]] -> [(Int, Char)] -> [String] -> Int
part1 rules base = length . filter isRight . map (parse p "")
  where
    p = build 0 <* eof

    build :: Int -> Parsec String () String
    build i = case lookup i base of
      Just c -> string [c]
      Nothing -> choice $ map (try . mconcat . map build) $ rules ! i

part2 :: Map Int [[Int]] -> [(Int, Char)] -> [String] -> Int
part2 rules base = length . filter isRight . map (parse p "")
  where
    p42 = build 42
    p31 = build 31
    p = do
      a <- many1 (try p42)
      b <- many1 p31
      eof
      if length a > length b then return () else fail "no"

    build :: Int -> Parsec String () String
    build i = case lookup i base of
      Just c -> string [c]
      Nothing -> choice $ map (try . mconcat . map build) $ rules ! i

-- Parsing

inputP :: Parsec String () (Map Int [[Int]], [(Int, Char)], [String])
inputP = do
  references <- Map.fromList <$> sepEndBy referenceP newline
  newline
  base <- sepEndBy baseP newline
  newline
  samples <- sepEndBy (many (oneOf "ab")) newline
  return (references, base, samples)

referenceP :: Parsec String () (Int, [[Int]])
referenceP = do
  n <- intP
  string ": "
  poss <- sepBy1 (sepEndBy1 intP (char ' ')) (string "| ")
  return (n, poss)

baseP :: Parsec String () (Int, Char)
baseP = (,) <$> intP <*> between (string ": \"") (string "\"") (oneOf "ab")

intP :: Parsec String () Int
intP = read <$> many1 digit

ex = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb\n"

ex2 =
  unlines
    [ "0: 8 11",
      "8: 42",
      "11: 42 31",
      "42: 9 14 | 10 1",
      "9: 14 27 | 1 26",
      "10: 23 14 | 28 1",
      "5: 1 14 | 15 1",
      "19: 14 1 | 14 14",
      "12: 24 14 | 19 1",
      "16: 15 1 | 14 14",
      "31: 14 17 | 1 13",
      "6: 14 14 | 1 14",
      "2: 1 24 | 14 4",
      "13: 14 3 | 1 12",
      "15: 1 | 14",
      "17: 14 2 | 1 7",
      "23: 25 1 | 22 14",
      "28: 16 1",
      "4: 1 1",
      "20: 14 14 | 1 15",
      "3: 5 14 | 16 1",
      "27: 1 6 | 14 18",
      "21: 14 1 | 1 14",
      "25: 1 1 | 1 14",
      "22: 14 14",
      "26: 14 22 | 1 20",
      "18: 15 15",
      "7: 14 5 | 1 21",
      "24: 14 1",
      "",
      "1: \"a\"",
      "14: \"b\"",
      "",
      "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
      "bbabbbbaabaabba",
      "babbbbaabbbbbabbbbbbaabaaabaaa",
      "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
      "bbbbbbbaaaabbbbaaabbabaaa",
      "bbbababbbbaaaaaaaabbababaaababaabab",
      "ababaaaaaabaaab",
      "ababaaaaabbbaba",
      "baabbaaaabbaaaababbaababb",
      "abbbbabbbbaaaababbbbbbaaaababb",
      "aaaaabbaabaaaaababaa",
      "aaaabbaaaabbaaa",
      "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
      "babaaabbbaaabaababbaabababaaab",
      "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
    ]
