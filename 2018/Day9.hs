import Data.Map (Map, (!))
import qualified Data.Map as M

data Circle a = Circle [a] a [a] deriving Show

move :: Int -> Circle a -> Circle a
move 0 c = c
move _ (Circle [] a []) = Circle [] a []
move n (Circle [] a cw)  | n < 0 = move n (Circle (reverse cw) a [])
move n (Circle ccw a []) | n > 0 = move n (Circle [] a (reverse ccw))
move n (Circle ccw a cw)
  | n < 0  = move (n+1) (Circle (tail ccw) (head ccw) (a:cw) )
  | otherwise = move (n-1) (Circle (a:ccw) (head cw) (tail cw) )

insert :: a -> Circle a -> Circle a
insert a (Circle ccw c cw) = Circle ccw a (c:cw)

pop :: Circle a -> (a, Circle a)
pop (Circle ccw a [])     = pop $ Circle [] a (reverse ccw)
pop (Circle ccw a (c:cw)) = (a, Circle ccw c cw)

main = do
  let player = 411
      marble = 7117000
  print $ play player marble

play :: Int -> Int -> Int
play player marble = go 1 (Circle [] 0 []) score0
  where
    score0 = M.fromList $ zip [0..player-1] $ repeat 0
    go n circle score
      | n == marble = maximum $ map snd $ M.toList score
      | mod n 23 == 0 = go (n+1) circle' (M.adjust (+(n+a)) (mod (n-1) player) score)
      | otherwise = go (n+1) (insert n $ move 2 circle) score
      where
        (a, circle') = pop $ move (-7) circle
