module Day07 where

import Data.Graph
import Text.Parsec

main :: IO ()
main = do
  rawRules <- readFile "Day07.txt"
  --   let rawRules = ex
  let Right rules = parse rulesP "rules" rawRules
  print $ part1 rules
  print $ part2 rules

part1 :: [Rule] -> Int
part1 rules = length (reachable (transposeG graph) goal) - 1
  where
    Just goal = vertexFromKey "shiny gold"
    (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ map (\(Rule x nodes) -> (x, x, map snd nodes)) rules

-- part2 :: [Rule] -> Int
part2 rules = graph
  where
    [graph] = stronglyConnComp $ map (\(Rule x nodes) -> (x, x, map snd nodes)) rules

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