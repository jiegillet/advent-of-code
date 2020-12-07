module Day07 where

import Control.Monad (foldM)
import qualified Control.Monad.State as S
import Data.Graph (graphFromEdges, reachable, transposeG)
import qualified Data.Map as Map
import Text.Parsec
  ( Parsec,
    digit,
    many,
    noneOf,
    parse,
    sepBy1,
    sepEndBy1,
    space,
    string,
    (<|>),
  )

main :: IO ()
main = do
  rawRules <- readFile "Day07.txt"
  -- let rawRules = ex
  let Right rules = parse rulesP "rules" rawRules
  print $ part1 rules
  print $ part2 rules

part1 :: [Rule] -> Int
part1 rules = length (reachable (transposeG graph) goal) - 1
  where
    Just goal = vertexFromKey "shiny gold"
    (graph, _, vertexFromKey) = graphFromEdges $ map (\(Rule x nodes) -> (x, x, map snd nodes)) rules

part2 :: [Rule] -> Int
part2 rules = subtract 1 $ S.evalState (countBag "shiny gold") Map.empty
  where
    countBag :: String -> S.State (Map.Map String Int) Int
    countBag bag = do
      bags <- S.get
      case Map.lookup bag bags of
        Just n -> return n
        Nothing -> case lookup bag (map (\(Rule x nodes) -> (x, nodes)) rules) of
          Nothing -> return 1
          Just nodes -> do
            count <- foldM (\total (numBag, insideBag) -> (+ total) . (* numBag) <$> countBag insideBag) 1 nodes
            S.modify (Map.insert bag count)
            return count

data Rule = Rule String [(Int, String)] deriving (Show)

rulesP :: Parsec String () [Rule]
rulesP = sepEndBy1 ruleP (string ".\n")

ruleP :: Parsec String () Rule
ruleP = do
  shade <- many (noneOf " ")
  space
  color <- many (noneOf " ")
  string " bags contain "
  bags <- (string "no other bags" >> return []) <|> sepBy1 insideBagP (string ", ")
  return $ Rule (shade ++ " " ++ color) bags

insideBagP :: Parsec String () (Int, String)
insideBagP = do
  num <- read <$> many digit
  space
  shade <- many (noneOf " ")
  space
  color <- many (noneOf " ")
  many (noneOf ",.")
  return (num, shade ++ " " ++ color)

ex =
  unlines
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      "bright white bags contain 1 shiny gold bag.",
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      "faded blue bags contain no other bags.",
      "dotted black bags contain no other bags."
    ]