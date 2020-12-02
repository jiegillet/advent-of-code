module Day02 where

import Text.Parsec
import Text.Parsec.Char
import Data.Bits (xor)
main :: IO ()
main = do
  passwords <- readFile "Day02.txt"
  let Right pass = parse passwordsP "passwordP" passwords
  print $ part1 pass
  print $ part2 pass

data Password = Password Char Int Int String   deriving Show

valid1 :: Password -> Bool
valid1 (Password char low high password) = 
  let n = length $ filter (==char) password
  in low <= n && n <= high

part1 :: [Password] -> Int
part1 = length . filter valid1

valid2 :: Password -> Bool
valid2 (Password char low high password) =  (password!!(low-1) == char) `xor` (password!!(high-1) == char)

part2 :: [Password] -> Int
part2 = length . filter valid2

passwordsP :: Parsec String () [Password]
passwordsP = sepBy passwordP  newline

passwordP :: Parsec String () Password
passwordP = do
  low <- read <$> many digit
  _ <- char '-'
  high <- read <$> many digit
  _ <- space
  chr <- anyChar
  _ <- string ": "
  password <- many (noneOf "\n")
  return $ Password chr low high password
