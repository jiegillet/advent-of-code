module Main where

import Data.List (isPrefixOf, sortOn, transpose)
import qualified Data.Set as Set
import Text.Parsec
  ( Parsec,
    char,
    digit,
    many,
    manyTill,
    newline,
    noneOf,
    parse,
    sepBy,
    sepBy1,
    string,
  )

-- Types

data Rule = Rule String [(Int, Int)] deriving (Show)

getField :: Rule -> String
getField (Rule field _) = field

type Ticket = [Int]

main :: IO ()
main = do
  input <- readFile "Day16.txt"
  let Right (rules, myTicket, otherTickets) = parse inputP "input" input
  print $ part1 rules otherTickets
  print $ part2 rules myTicket otherTickets

part1 :: [Rule] -> [Ticket] -> Int
part1 rules = sum . concatMap invalidValues
  where
    invalidValues = filter (\n -> all (not . followsRule n) rules)

followsRule :: Int -> Rule -> Bool
followsRule n (Rule _ ranges) = any check ranges
  where
    check (low, high) = (low <= n) && (n <= high)

part2 :: [Rule] -> Ticket -> [Ticket] -> Int
part2 rules myTicket tickets = toInt . departureIndices . eliminate . probableFields $ validTickets
  where
    validTickets = myTicket : filter (all (\n -> any (followsRule n) rules)) tickets

    probableFields = sortOn (Set.size . snd) . zip [0 ..] . map validRules . transpose

    validRules field = Set.fromList $ map getField $ filter (\r -> all (flip followsRule r) field) rules

    eliminate [] = []
    eliminate ((i, singleton) : others) =
      (i, Set.elemAt 0 singleton) : (eliminate $ map (fmap (flip Set.difference singleton)) others)

    departureIndices = map fst . filter (isPrefixOf "departure" . snd)

    toInt = product . map (myTicket !!)

-- Parser

inputP :: Parsec String () ([Rule], Ticket, [Ticket])
inputP =
  (,,) <$> manyTill ruleP newline
    <*> (string "your ticket:\n" >> ticketP)
    <*> (string "\nnearby tickets:\n" >> many ticketP)

ruleP :: Parsec String () Rule
ruleP = do
  field <- many (noneOf ":")
  string ": "
  ranges <- sepBy ((,) <$> intP <*> (char '-' >> intP)) (string " or ")
  newline
  return $ Rule field ranges

ticketP :: Parsec String () Ticket
ticketP = sepBy1 intP (char ',') <* newline

intP :: Parsec String () Int
intP = read <$> many digit