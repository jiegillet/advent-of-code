import Data.Bits
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char (isDigit)
import Data.Map (Map, (!))
import Data.Set (Set)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Clay | Flow | Still deriving (Eq, Show)


-- from flow tiles
flow :: [(Int, Int)] -> State (Map (Int, Int) Tile) Int
flow sources = do
  m <- get
  down <- mapM flowDown sources
  return 0


-- flow tile falling down
flowDown :: (Int, Int) -> State (Map (Int, Int) Tile) Int
flowDown (i, j) = do
  m <- get
  let down = M.filterWithKey (\(i', j') t -> i==i') m
  return 0

main = do
  txt <- readFile "Day17.txt"
  let m = parse txt
  print $ evalState (flow [(500,0)]) m


parse :: String -> Map (Int, Int) Tile
parse = fst . last . readP_to_S parseP

parseP :: ReadP (Map (Int, Int) Tile)
parseP = M.unions <$> many lineP

lineP :: ReadP (Map (Int, Int) Tile)
lineP = do
  c <- choice [char 'x', char 'y']
  string "="
  x <- read <$> munch isDigit
  choice [string ", x=", string ", y="]
  y0 <- read <$> munch isDigit
  string ".."
  y1 <- read <$> munch isDigit
  char '\n'
  if c == 'x'
    then return $ M.fromList [((x, j), Clay) | j <- [y0..y1]]
    else return $ M.fromList [((j, x), Clay) | j <- [y0..y1]]

ex = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"
