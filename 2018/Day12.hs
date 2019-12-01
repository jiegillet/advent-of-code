import Data.List (findIndex, findIndices, transpose)
import Data.Map (Map, (!))
import qualified Data.Map as M

data Pot = Dead | Alive deriving (Show, Eq, Ord)

toPot :: Char -> Pot
toPot '.' = Dead
toPot '#' = Alive

toRules :: [String] -> Map [Pot] Pot
toRules = M.fromList . map rules
  where
    rules :: String -> ([Pot], Pot)
    rules s = (map toPot $ take 5 s, toPot $ last s)

evolve :: Map [Pot] Pot -> [Pot] -> [Pot]
evolve rules p = map (rules!) $ transpose [ Dead : Dead : init (init p)
                                          , Dead : init p
                                          , p
                                          , tail p ++ [Dead]
                                          , drop 2 p ++ [Dead, Dead]
                                          ]

pad :: Int -> [Pot] -> [Pot]
pad n = (++ replicate n Dead) . (replicate n Dead ++)

score :: Int -> [Pot] -> Int
score padding = sum . map (subtract padding) . findIndices (==Alive)

main = do
  (start':_:rules') <- lines <$> readFile "../input/Day12.txt"
  let rules = toRules rules'
      start = map toPot $ drop 15 start'
      stop = (!!20) $ iterate (evolve rules) $ pad 25 start
  print $ score 25 stop

  let far = (!!500) $ iterate (evolve rules) $ pad 505 start
      further = evolve rules far
      Just speed = subtract <$> findIndex (==Alive) far
                            <*> findIndex (==Alive) further
      alive = length $ filter (==Alive) far
  print $ (score 505 far) + speed * alive * (50000000000 - 500)
