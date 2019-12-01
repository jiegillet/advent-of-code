import Control.Monad.State
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Tile
  = Elf Int
  | Goblin Int
  | Open
  | Wall
  deriving (Eq, Show)

hp :: Tile -> Int
hp (Elf n)    = n
hp (Goblin n) = n

ally, ennemy :: Tile -> Tile -> Bool
ennemy (Elf _) (Goblin _) = True
ennemy (Goblin _) (Elf _) = True
ennemy _ _                = False
ally (Elf _) (Elf _)       = True
ally (Goblin _) (Goblin _) = True
ally _ _                   = False

isCreature :: Tile -> Bool
isCreature Open = False
isCreature Wall = False
isCreature _    = True

isElf, isGoblin :: Tile -> Bool
isElf (Elf _) = True
isElf _       = False
isGoblin (Goblin _) = True
isGoblin _          = False

hit :: Tile -> Tile
hit (Elf n)    = Elf (n-3)
hit (Goblin n) = Goblin (n-3)
hit e          = e

data Move
  = Stuck
  | Attack
  | Move (Int, Int) (Int, Int)
  deriving Show

type World = Map (Int, Int) Tile

getWorld :: String -> World
getWorld = M.fromList . concatMap horizontal . zip [1..] . lines
  where
    horizontal (i, s) = map (toTile i) $ zip [1..] s
    toTile i (j, '.') = ((i, j), Open)
    toTile i (j, '#') = ((i, j), Wall)
    toTile i (j, 'E') = ((i, j), Elf 200)
    toTile i (j, 'G') = ((i, j), Goblin 200)

next :: World -> ((Int, Int), Tile) -> Move
next m ((i, j), t) = go (S.singleton (i, j)) start
  where
    start = map (\f -> Move f f) [(i-1,j),(i,j-1),(i,j+1),(i+1,j)]
    neighbors f (i, j) = map (Move f) [(i-1,j),(i,j-1),(i,j+1),(i+1,j)]

    go :: Set (Int, Int) -> [Move] -> Move
    go _ [] = Stuck
    go visited (Move f k: rest)
      | S.member k visited       = go visited rest
      | m!k == Wall              = go visited rest
      | ally (m!k) t             = go visited rest
      | k == f && ennemy (m!k) t = Attack
      | ennemy (m!k) t           = Move f k
      | otherwise            = go (S.insert k visited) (rest ++ neighbors f k)

move :: ((Int, Int), Tile) -> State World ()
move (k, t) = do
  m <- get
  if not $ ally (m!k) t
    then return ()
    else case next m (k, t) of
      Stuck    -> return ()
      Move f _ -> do
        modify (M.adjust (const t) f . M.adjust (const Open) k)
        attack (f, t)
      Attack   -> attack (k, t)



attack :: ((Int, Int), Tile) -> State World ()
attack ((i, j), t) = do
  m <- get
  case filter (ennemy t . (m!)) [(i-1,j),(i,j-1),(i,j+1),(i+1,j)] of
    []     -> return ()
    targets -> do
      let weakest = minimumBy (compare `on` hp . (m!)) targets
      if hp (m!weakest) > 3
        then modify (M.adjust hit weakest)
        else modify (M.adjust (const Open) weakest)

playRounds :: Int -> State World Int
playRounds n = do
  m <- get
  mapM_ move (M.toList $ M.filter isCreature m)
  let goblins = M.toList $ M.filter isGoblin m
      elves = M.toList $ M.filter isElf m
  if null elves || null goblins
    then return $ (n-1) * sum (map (hp . snd) $ elves ++ goblins)
    else playRounds (n + 1)
    -- else return (n + 1)

printWorld :: World -> String
-- printWorld w = unlines [[ char (w!(i, j)) | j <- [1..32]] | i <- [1..32]]
printWorld w = unlines [[ char (w!(i, j)) | j <- [1..7]] | i <- [1..7]]
  where
    char Open       = '.'
    char Wall       = '#'
    char (Elf _)    = 'E'
    char (Goblin _) = 'G'

main = do
  txt <- readFile "../input/Day15.txt"
  let w = getWorld txt
      -- n = 47
  -- putStrLn $ printWorld w
  -- mapM_ (putStrLn . printWorld) $ take 39 $ iterate (execState (playRounds 0)) w
  -- print $ fmap (M.filter isCreature) $ runState (mapM playRounds [0..n-1]) w
  -- putStrLn $ printWorld $ execState (mapM playRounds [0..n-1]) w
  -- putStrLn $ printWorld $ execState (playRounds 0) w
  -- print $ evalState (playRounds 0) w
  print $ fmap (M.filter isCreature) $ runState (playRounds 0) w
