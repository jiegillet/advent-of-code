{-# LANGUAGE LambdaCase #-}

import Data.Map (Map, (!))
import qualified Data.Map as M

data Track = Hor | Vert | DiagL | DiagR | Cross deriving (Show, Eq)

data Dir = L | R | U | D deriving (Show, Eq)

data Turn = Lft | Str | Rgt

type Carts = Map (Int, Int) (Dir, [Turn])
type Tracks = Map (Int, Int) Track

toTrack :: String -> Tracks
toTrack s = M.fromList $ concat $ zipWith row [0..] $ map (zip [0..]) $ lines s
  where
    row i = map track . filter (flip elem "|-+\\/v^<>" . snd)
      where
        track (j, '|')  = ((i, j), Vert)
        track (j, '^')  = ((i, j), Vert)
        track (j, 'v')  = ((i, j), Vert)
        track (j, '-')  = ((i, j), Hor)
        track (j, '<')  = ((i, j), Hor)
        track (j, '>')  = ((i, j), Hor)
        track (j, '/')  = ((i, j), DiagR)
        track (j, '\\') = ((i, j), DiagL)
        track (j, '+')  = ((i, j), Cross)

toCarts :: String -> Carts
toCarts s = M.fromList $ concat $ zipWith row [0..] $ map (zip [0..]) $ lines s
  where
    row i = map track . filter (flip elem "v^<>" . snd)
      where
        track (j, '^') = ((i, j), (U, cycle [Lft, Str, Rgt]))
        track (j, 'v') = ((i, j), (D, cycle [Lft, Str, Rgt]))
        track (j, '<') = ((i, j), (L, cycle [Lft, Str, Rgt]))
        track (j, '>') = ((i, j), (R, cycle [Lft, Str, Rgt]))

evolve :: Tracks -> Carts -> ([(Int, Int)], Carts)
evolve tracks carts = go ([], carts) (M.toList carts)
  where
    go x [] = x
    go (crash, carts) ((ij, dir):rest)
      | M.member ij' carts = go (ij': crash, M.delete ij' $ M.delete ij carts) $ filter ((/=ij') . fst) rest
      | otherwise = go (crash, M.insert ij' dir' $ M.delete ij carts) rest
      where
        (ij', dir') = move (ij, dir)

    move c = case c of
      ((i, j), (R, t)) | tracks ! (i, j+1) == Hor   -> ((i, j+1), (R, t))
      ((i, j), (R, t)) | tracks ! (i, j+1) == DiagL -> ((i, j+1), (D, t))
      ((i, j), (R, t)) | tracks ! (i, j+1) == DiagR -> ((i, j+1), (U, t))
      ((i, j), (R, Lft:ts))                         -> ((i, j+1), (U, ts))
      ((i, j), (R, Str:ts))                         -> ((i, j+1), (R, ts))
      ((i, j), (R, Rgt:ts))                         -> ((i, j+1), (D, ts))
      ((i, j), (L, t)) | tracks ! (i, j-1) == Hor   -> ((i, j-1), (L, t))
      ((i, j), (L, t)) | tracks ! (i, j-1) == DiagL -> ((i, j-1), (U, t))
      ((i, j), (L, t)) | tracks ! (i, j-1) == DiagR -> ((i, j-1), (D, t))
      ((i, j), (L, Lft:ts))                         -> ((i, j-1), (D, ts))
      ((i, j), (L, Str:ts))                         -> ((i, j-1), (L, ts))
      ((i, j), (L, Rgt:ts))                         -> ((i, j-1), (U, ts))
      ((i, j), (U, t)) | tracks ! (i-1, j) == Vert  -> ((i-1, j), (U, t))
      ((i, j), (U, t)) | tracks ! (i-1, j) == DiagL -> ((i-1, j), (L, t))
      ((i, j), (U, t)) | tracks ! (i-1, j) == DiagR -> ((i-1, j), (R, t))
      ((i, j), (U, Lft:ts))                         -> ((i-1, j), (L, ts))
      ((i, j), (U, Str:ts))                         -> ((i-1, j), (U, ts))
      ((i, j), (U, Rgt:ts))                         -> ((i-1, j), (R, ts))
      ((i, j), (D, t)) | tracks ! (i+1, j) == Vert  -> ((i+1, j), (D, t))
      ((i, j), (D, t)) | tracks ! (i+1, j) == DiagL -> ((i+1, j), (R, t))
      ((i, j), (D, t)) | tracks ! (i+1, j) == DiagR -> ((i+1, j), (L, t))
      ((i, j), (D, Lft:ts))                         -> ((i+1, j), (R, ts))
      ((i, j), (D, Str:ts))                         -> ((i+1, j), (D, ts))
      ((i, j), (D, Rgt:ts))                         -> ((i+1, j), (L, ts))

main = do
  txt <- readFile "../input/Day13.txt"
  let tracks = toTrack txt
      carts = toCarts txt
      nCarts = M.size carts
      evol = iterate (>>= evolve tracks) ([], carts)
  putStrLn "Coordinates are inverted"
  print $ fst $ head $ dropWhile (null . fst) evol
  print $ head $ dropWhile ((>1) . length) $ map (M.map fst . snd) evol


ex = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "
