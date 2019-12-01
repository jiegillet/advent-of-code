import Data.Sequence (Seq (..), ViewR (..), (|>))
import qualified Data.Sequence as S

data Recipe = Recipe
  { i :: Int
  , j :: Int
  , s :: Seq Int
  } deriving Show

evolve :: Recipe -> Recipe
evolve (Recipe i j s) = Recipe i' j' s'
  where
    a = S.index s i
    b = S.index s j
    n = a + b
    s' = if n < 10 then s |> n else s |> 1  |> (mod n 10)
    i' = (i + a + 1) `mod` S.length s'
    j' = (j + b + 1) `mod` S.length s'

last10 :: Recipe -> String
last10 (Recipe _ _ s) = foldr ((++) . show) "" $ S.drop (S.length s - 10) s

isSuffix :: Eq a => Seq a -> Seq a -> Bool
isSuffix a b = case (S.viewr a, S.viewr b) of
  (EmptyR, _)                 -> True
  (as :> a, bs :> b) | a == b -> isSuffix as bs
  _                           -> False

isIn :: Eq a => Seq a -> Seq a -> Bool
isIn a b = isSuffix a b || isSuffix (a |> S.index b (S.length b - 1)) b

main = do
  let start = Recipe 0 1 (S.fromList [3, 7])
      evol = iterate evolve start
  putStrLn $ last10 $ head $ dropWhile ((<380621 + 10) . S.length . s) evol
  let goal = S.fromList [3, 8, 0, 6, 2, 1]
  print $ subtract 7 $ S.length $ s $ head $ dropWhile (not . isIn goal . s) evol
