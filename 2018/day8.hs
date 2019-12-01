{-# LANGUAGE FlexibleInstances #-}

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  txt <- readFile "../input/Day8.txt"
  let tree = read txt :: Tree Int
  print $ check tree
  print $ value tree

data Tree a = Node [a] [Tree a] deriving Show

instance Read (Tree Int) where
  readsPrec _ = readP_to_S treeP

intP :: ReadP Int
intP = read <$> munch1 isDigit

treeP :: ReadP (Tree Int)
treeP = do
  nChild <- intP
  skipSpaces
  nMeta <- intP
  skipSpaces
  children <- count nChild treeP
  meta <- count nMeta (intP <* skipSpaces)
  return $ Node meta children

check :: Tree Int -> Int
check (Node meta child) = sum $ meta ++ map check child

value :: Tree Int -> Int
value (Node meta []) = sum meta
value (Node meta child) =
  sum $ map value $ map ((child!!) . pred) $ filter index meta
  where
    size = length child
    index n
      | n < 1 = False
      | n > size = False
      | otherwise = True

ex = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
