module Main where

import Text.Parsec
  ( Parsec,
    between,
    chainl1,
    char,
    digit,
    newline,
    optional,
    parse,
    sepEndBy,
    string,
    (<|>),
  )

main :: IO ()
main = do
  input <- readFile "Day18.txt"
  let Right exprs = parse (sepEndBy exprP newline) "input" input
  print $ sum $ map eval exprs
  let Right exprs = parse (sepEndBy exprP' newline) "input" input
  print $ sum $ map eval exprs

-- Expressions

data Expr
  = Val Int
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Val i) = i
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

-- Parser Part 1

exprP, termP :: Parsec String () Expr
exprP = termP `chainl1` (addP <|> multP)
termP = parenP <|> valueP

valueP, parenP :: Parsec String () Expr
valueP = Val . read . pure <$> (digit <* optional (char ' '))
parenP = between (char '(') (char ')' <* optional (char ' ')) exprP

addP, multP :: Parsec String () (Expr -> Expr -> Expr)
addP = Add <$ string "+ "
multP = Mult <$ string "* "

-- Parser Part 2

exprP', termP', parenP' :: Parsec String () Expr
exprP' = (termP' `chainl1` addP) `chainl1` multP
termP' = parenP' <|> valueP
parenP' = between (char '(') (char ')' <* optional (char ' ')) exprP'
