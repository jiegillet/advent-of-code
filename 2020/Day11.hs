{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map

main :: IO ()
main = do
  seats <- map (map toSeat) . lines <$> readFile "Day11.txt"
  print $ part1 seats
  print $ part2 seats

data Seat = Empty | Occupied | Floor deriving (Show, Eq)

toSeat '.' = Floor
toSeat 'L' = Empty

part1 :: [[Seat]] -> Int
part1 seats = countOccupied $ fst $ head $ filter (uncurry (==)) $ zip evol (tail evol)
  where
    evol = iterate step1 seats
    countOccupied = sum . map (sum . map count)

count :: Seat -> Int
count Occupied = 1
count _ = 0

step1 :: [[Seat]] -> [[Seat]]
step1 seats = zipWith (zipWith change) neighbors seats
  where
    floorRow = replicate (length $ head seats) Floor
    shiftUp = (floorRow :) . init
    shiftDown = (++ [floorRow]) . tail
    shiftRight = map ((Floor :) . init)
    shiftLeft = map ((++ [Floor]) . tail)

    shifts =
      [ shiftUp seats,
        shiftDown seats,
        shiftLeft seats,
        shiftRight seats,
        shiftLeft $ shiftUp seats,
        shiftRight $ shiftUp seats,
        shiftLeft $ shiftDown seats,
        shiftRight $ shiftDown seats
      ]

    neighbors = foldr addUp (map (map (const 0)) seats) shifts
    addUp s = zipWith (zipWith (+)) (map (map count) s)

    change _ Floor = Floor
    change 0 Empty = Occupied
    change n Occupied = if n >= 4 then Empty else Occupied
    change _ seat = seat

part2 :: [[Seat]] -> Int
part2 seatsList = countOccupied $ fst $ head $ filter (uncurry (==)) $ zip evol (tail evol)
  where
    seats = Map.fromList $ concat $ zipWith (\i row -> zipWith (\j s -> ((i, j), s)) [0 ..] row) [0 ..] seatsList
    evol = iterate step2 seats
    countOccupied = Map.size . Map.filter (== Occupied)

step2 :: Map.Map (Int, Int) Seat -> Map.Map (Int, Int) Seat
step2 seats = Map.mapWithKey change seats
  where
    (i0, j0) = fst $ Map.findMin seats
    (iN, jN) = fst $ Map.findMax seats
    neighbors = sum . map (countFirst . dropWhile (== Floor) . map (seats Map.!)) . directions
    directions (i, j) =
      [ [(i', j) | i' <- [i + 1 .. iN]], -- Right
        [(i', j) | i' <- [i -1, i -2 .. i0]], -- Left
        [(i, j') | j' <- [j -1, j -2 .. j0]], -- Up
        [(i, j') | j' <- [j + 1 .. jN]], -- Down
        [(i + k, j + k) | k <- [1 .. min (iN - i) (jN - j)]],
        [(i + k, j - k) | k <- [1 .. min (iN - i) (j - j0)]],
        [(i - k, j + k) | k <- [1 .. min (i - i0) (jN - j)]],
        [(i - k, j - k) | k <- [1 .. min (i - i0) (j - j0)]]
      ]

    countFirst [] = 0
    countFirst (Empty : _) = 0
    countFirst (Occupied : _) = 1

    change _ Floor = Floor
    change (i, j) Empty = if neighbors (i, j) == 0 then Occupied else Empty
    change (i, j) Occupied = if neighbors (i, j) >= 5 then Empty else Occupied
