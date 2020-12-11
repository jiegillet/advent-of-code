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
    shiftRight = map (Floor :) . map init
    shiftLeft = map (++ [Floor]) . map tail

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

    neighbors = foldr addUp (map (map (const 0)) seats) $ shifts
    addUp s = zipWith (zipWith (+)) (map (map count) s)

    change _ Floor = Floor
    change 0 Empty = Occupied
    change n Occupied = if n >= 4 then Empty else Occupied
    change _ seat = seat

-- part2 :: [[Seat]] -> Int
part2 seatsList = seats
  where
    seats = Map.fromList $ concat $ zipWith (\i row -> zipWith (\j s -> ((i, j), s)) [0 ..] row) [0 ..] seatsList

step2 :: Map.Map (Int, Int) Seat -> Map.Map (Int, Int) Seat
step2 seats = undefined
