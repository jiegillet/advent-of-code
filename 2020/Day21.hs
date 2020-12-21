{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (intercalate, sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
  ( Parsec,
    char,
    many1,
    newline,
    noneOf,
    parse,
    sepBy,
    sepEndBy1,
    space,
    string,
  )

main :: IO ()
main = do
  input <- readFile "Day21.txt"
  let Right foods = parse inputP "input" input
  print $ solve foods

solve :: [(Set String, Set String)] -> (Int, String)
solve foods = (part1, part2)
  where
    part1 = sum $ map (Set.size . (Set.intersection safeIngredients) . fst) foods
    part2 = intercalate "," $ concatMap (Set.toList . snd) $ sortOn fst $ identifiedAllergens

    ingredients = mconcat $ map fst foods
    allergens = mconcat $ map snd foods

    identifiedAllergens = weedOut $ sortOn (Set.size . snd) $ map findCommonIngredients (Set.toList allergens)
    findCommonIngredients al = (al, foldr1 Set.intersection $ map fst $ filter ((al `Set.member`) . snd) foods)

    allergenicIngredients = mconcat $ map snd $ identifiedAllergens
    safeIngredients = Set.difference ingredients allergenicIngredients

    weedOut [] = []
    weedOut ((al, ingr) : rest)
      | Set.size ingr > 1 = error "this should not happen"
      | otherwise = (al, ingr) : weedOut (sortOn (Set.size . snd) $ map (fmap (`Set.difference` ingr)) rest)

-- Parsing

inputP :: Parsec String () [(Set String, Set String)]
inputP = sepEndBy1 foodP newline

foodP :: Parsec String () (Set String, Set String)
foodP = do
  ingredients <- sepEndBy1 (many1 $ noneOf " (") space
  string "(contains "
  allergens <- sepBy (many1 $ noneOf ",)") (string ", ")
  char ')'
  return (Set.fromList ingredients, Set.fromList allergens)
