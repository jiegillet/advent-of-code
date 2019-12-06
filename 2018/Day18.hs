import Data.Map ((!))
import qualified Data.Map as M

data Acre = Nil | Open | Tree | Lumber deriving (Eq, Show, Ord)
type Table a = [[a]]

apply :: (a -> b -> c) -> Table a -> Table b -> Table c
apply op = zipWith (zipWith op)

constTable :: a -> Table b -> Table a
constTable x = map (map (const x))

parse :: String -> Table Acre
parse = map (map toAcre) . lines
  where
    toAcre '.' = Open
    toAcre '|' = Tree
    toAcre '#' = Lumber

shiftX :: Int -> Table Acre -> Table Acre
shiftX n
  | n > 0 =  map (replicate n Nil ++)
  | otherwise = map ((++ replicate (abs n) Nil) . drop (abs n))

shiftY :: Int -> Table Acre -> Table Acre
shiftY n a
  | n > 0 = replicate n (map (const Nil) $ head a) ++ a
  | otherwise = drop (abs n) a ++ replicate (abs n) (map (const Nil) $ head a)

evolve :: Table Acre -> Table Acre
evolve a = apply new total a
  where
    start = constTable (0, 0, 0) a
    neighbors = [shiftX x $ shiftY y a | x <-[-1,0,1], y <-[-1,0,1], x /= 0 || y /=0]
    total = foldr count start neighbors
    count table c = apply count' c table
      where
        count' (o, t, l) Open = (o + 1, t, l)
        count' (o, t, l) Tree = (o, t + 1, l)
        count' (o, t, l) Lumber = (o, t, l + 1)
        count' c _ = c
    new (o, t, l) Open = if t >= 3 then Tree else Open
    new (o, t, l) Tree = if l >= 3 then Lumber else Tree
    new (o, t, l) Lumber = if l >= 1 && t >= 1 then Lumber else Open

ressource :: Table Acre -> Int
ressource a = (length $ filter (==Tree) $ concat a)
             * (length $ filter (==Lumber) $ concat a)

printAcres :: Table Acre -> String
printAcres = (++"\n\n") . unlines . map (map toChar)
  where
    toChar Open = '.'
    toChar Tree = '|'
    toChar Lumber = '#'

findCycle :: Table Acre -> Int
findCycle t = go 0 (M.empty) $ iterate evolve t
  where
    go n m (t:ts)
      | M.member t m = ressource $ findTree (m!t) n m
      | otherwise = go (n+1) (M.insert t n m) ts
    findTree i j = fst . head . M.toList . M.filter (==(i - 1 + mod (big - i + 1) (j - i)))
    big = 1000000000

main = do
  txt <- readFile "Day18.txt"
  let a = parse txt
      b = (!!10) $ iterate evolve a
  print $ ressource b
  print $ findCycle a


ex = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."
