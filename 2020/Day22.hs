module Main where

import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec
  ( Parsec,
    digit,
    many,
    many1,
    newline,
    parse,
    string,
  )

main :: IO ()
main = do
  input <- readFile "Day22.txt"
  let Right (p1, p2) = parse inputP "input" input
  print $ part1 p1 p2
  --   print $ part2 (S.fromList [9, 2, 6, 3, 1]) (S.fromList [5, 8, 4, 7, 10])
  --   print $ part2 (S.fromList [43, 19]) (S.fromList [2, 29, 14])
  print $ part2 p1 p2

part1 :: Seq Int -> Seq Int -> Int
part1 p1 = S.foldrWithIndex (\i c s -> s + ((i + 1) * c)) 0 . S.reverse . play p1
  where
    play Empty p2 = p2
    play p1 Empty = p1
    play (c1 :<| c1s) (c2 :<| c2s)
      | c1 > c2 = play (c1s |> c1 |> c2) c2s
      | otherwise = play c1s (c2s |> c2 |> c1)

part2 :: Seq Int -> Seq Int -> Int
part2 p1 = S.foldrWithIndex (\i c s -> s + ((i + 1) * c)) 0 . S.reverse . snd . play Set.empty p1
  where
    play :: Set (Seq Int, Seq Int) -> Seq Int -> Seq Int -> (Bool, Seq Int)
    play _ Empty p2 = (False, p2)
    play _ p1 Empty = (True, p1)
    play history p1@(c1 :<| c1s) p2@(c2 :<| c2s)
      | Set.member (p1, p2) history = (True, p1)
      | c1 <= S.length c1s,
        c2 <= S.length c2s =
        if fst (play Set.empty (S.take c1 c1s) (S.take c2 c2s))
          then play history' (c1s |> c1 |> c2) c2s
          else play history' c1s (c2s |> c2 |> c1)
      | c1 > c2 = play history' (c1s |> c1 |> c2) c2s
      | otherwise = play history' c1s (c2s |> c2 |> c1)
      where
        history' = Set.insert (p1, p2) history

-- Parsing

inputP :: Parsec String () (Seq Int, Seq Int)
inputP = do
  string "Player 1:\n"
  cards1 <- many (intP <* newline)
  newline
  string "Player 2:\n"
  cards2 <- many (intP <* newline)
  return (S.fromList cards1, S.fromList cards2)

intP :: Parsec String () Int
intP = read <$> many1 digit