module Day08 where

import Data.List ((\\))
import qualified Data.List as L
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Debug.Trace

main :: IO ()
main = do
  inputs <- map (fmap tail . L.splitAt 10 . words) . lines <$> readFile "Day08.txt"
  print $ part1 inputs
  print $ part2 inputs

part1 :: [([String], [String])] -> Int
part1 = sum . map (length . filter (`elem` [2, 3, 4, 7]) . map length . snd)

part2 :: [([String], [String])] -> Int
part2 = sum . map unentangle
  where
    start = foldr (`Map.insert` allSegments) Map.empty allSegments

    unentangle (inputs, outputs) =
      let narrowed = foldr narrowDown start inputs
          decodedMap = checkPossibilities inputs narrowed
          readOutputDigits = (reverseMapping !) . L.sort . map (decodedMap !)
       in read $ concatMap (show . readOutputDigits) outputs

allSegments :: [Char]
allSegments = "abcdefg"

segmentMapping :: Map Int [Char]
segmentMapping = Map.fromList [(0, "abcefg"), (1, "cf"), (2, "acdeg"), (3, "acdfg"), (4, "bcdf"), (5, "abdfg"), (6, "abdefg"), (7, "acf"), (8, "abcdefg"), (9, "abcdfg")]

reverseMapping :: Map [Char] Int
reverseMapping = Map.fromList $ map swap $ Map.toList segmentMapping

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

narrowDown :: String -> Map Char [Char] -> Map Char [Char]
narrowDown input
  | length input == length matches = remove . intersect
  | otherwise = id
  where
    matches = concat $ Map.elems $ Map.filter ((== length input) . length) segmentMapping

    intersect poss = foldr (Map.update (Just . (`L.intersect` matches))) poss input
    remove poss = foldr (Map.update (Just . (\\ matches))) poss (allSegments \\ input)

checkPossibilities :: [String] -> Map Char [Char] -> Map Char Char
checkPossibilities inputs possibilities =
  head
    [ choice
      | a <- possibilities ! 'a',
        b <- possibilities ! 'b',
        c <- possibilities ! 'c',
        d <- possibilities ! 'd',
        e <- possibilities ! 'e',
        f <- possibilities ! 'f',
        g <- possibilities ! 'g',
        L.nub [a, b, c, d, e, f, g] == [a, b, c, d, e, f, g],
        let choice = Map.fromList $ zip allSegments [a, b, c, d, e, f, g],
        L.all (\input -> L.sort (map (choice !) input) `Map.member` reverseMapping) inputs
    ]
