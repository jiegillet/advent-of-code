module Day14 where

import Data.List (unfoldr)
import Text.Parsec
import Text.Parsec.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio

main :: IO ()
main = do
  raw <- readFile "Day14.txt"
  let Right reactions = parse reactionsP "" raw
  -- let Right reactions = parse reactionsP "" test2
  print $ part1 reactions
  print $ part2 reactions

type Chemical = (String, Integer)

part1 :: Map Chemical [Chemical] -> Integer
part1 reactions = quantity!"ORE"
  where
    baseQuantity :: Map String Integer
    baseQuantity = Map.fromList $ Map.keys reactions

    quantity :: Map String Integer
    quantity = foldr breakdown (Map.singleton "FUEL" 1) (hierarchy reactions)

    breakdown :: Set String -> Map String Integer -> Map String Integer
    breakdown layer needed = Map.foldrWithKey create needed needed
      where
        create chem n need
          | Set.member chem layer = Map.unionWith (+) (ingredients chem n) need'
          | otherwise = need
          where
            need' = Map.delete chem need

    ingredients :: String -> Integer -> Map String Integer
    ingredients chem n = (*k) <$> Map.fromList (reactions!(chem, b))
      where
        b = baseQuantity!chem
        k = div (n + b - 1) b

part2 :: Map Chemical [Chemical] -> Integer
part2 reactions = floor $ 1000000000000 / (quantity!"ORE")
  where
    baseQuantity :: Map String Integer
    baseQuantity = Map.fromList $ Map.keys reactions

    quantity :: Map String (Ratio Integer)
    quantity = foldr breakdown (Map.singleton "FUEL" 1) (hierarchy reactions)

    breakdown :: Set String -> Map String (Ratio Integer) -> Map String (Ratio Integer)
    breakdown layer needed = Map.foldrWithKey create needed needed
      where
        create chem n need
          | Set.member chem layer = Map.unionWith (+) (ingredients chem n) need'
          | otherwise = need
          where
            need' = Map.delete chem need

    ingredients :: String -> Ratio Integer -> Map String (Ratio Integer)
    ingredients chem n =  (*n) . (%b) <$> Map.fromList (reactions!(chem, b))
      where
        b = baseQuantity!chem

hierarchy :: Map Chemical [Chemical] -> [Set String]
hierarchy reactions = unfoldr mkHierarchy (Set.singleton "ORE")
  where
    mkHierarchy :: Set String -> Maybe (Set String, Set String)
    mkHierarchy lower
      | Set.null layer = Nothing
      | otherwise      = Just (layer, Set.union lower higher)
      where higher = Set.fromList
                    $ map fst
                    $ Map.keys
                    $ Map.filter (all (flip Set.member lower) . map fst)
                    $ reactions
            layer = Set.difference higher lower


reactionsP :: Parsec String () (Map Chemical [Chemical])
reactionsP = Map.fromList <$> sepEndBy reacP (char '\n')

chemP :: Parsec String () Chemical
chemP =  do
  n <- read <$> many digit
  space
  name <- many $ noneOf " ,\n"
  return (name, n)

reacP :: Parsec String () (Chemical, [Chemical])
reacP = do
  input <- sepBy chemP (string ", ")
  string " => "
  output <- chemP
  return (output, input)


test = "157 ORE => 5 A\n165 ORE => 6 B\n179 ORE => 7 C\n177 ORE => 5 D\n165 ORE => 2 E\n7 B, 7 C => 2 BC\n12 D, 1 E, 8 C => 9 DEC\n3 B, 7 A, 5 D, 10 C => 8 ABCD\n44 BC, 5 ABCD, 1 DEC, 29 A, 9 E, 48 D => 1 FUEL"
test2 = "139 ORE => 4 A\n144 ORE => 7 B\n145 ORE => 6 C\n176 ORE => 6 D\n1 A => 8 E\n17 A, 3 B => 8 AB\n22 D, 37 C => 5 CD\n1 D, 6 C => 4 DC\n2 AB, 7 CD, 2 E, 11 C => 1 X\n5 C, 7 DC, 2 CD, 2 AB, 19 E => 3 Y\n5 D, 7 C, 9 AB, 37 E => 6 Z\n53 X, 6 C, 46 D, 81 Y, 68 E, 25 Z => 1 FUEL\n"
