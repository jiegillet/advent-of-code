module Day09 where

import qualified Data.List as L
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  floor <- getFloor . map (map (read . pure)) . lines <$> readFile "Day09.txt"
  print $ part1 floor
  print $ part2 floor

getFloor :: [[Int]] -> Map (Int, Int) Int
getFloor = Map.fromList . concat . zipWith (\r row -> zipWith (\c n -> ((r, c), n)) [0 ..] row) [0 ..]

part1 :: Map (Int, Int) Int -> Int
part1 floor = sum $ map ((+ 1) . (floor !)) $ findMinima floor

findMinima :: Map (Int, Int) Int -> [(Int, Int)]
findMinima floor =
  [ coord
    | coord <- Map.keys floor,
      all (\coord' -> floor ! coord < floor ! coord') (findNeighbors floor coord)
  ]

findNeighbors :: Map (Int, Int) a -> (Int, Int) -> [(Int, Int)]
findNeighbors floor (r, c) =
  [ (r + dr, c + dc)
    | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
      Map.member (r + dr, c + dc) floor
  ]

data Floor
  = Wall
  | Basin (Int, Int)
  | Unknown
  deriving (Eq, Ord, Show)

isBasin :: Floor -> Bool
isBasin (Basin _) = True
isBasin _ = False

part2 :: Map (Int, Int) Int -> Int
part2 floor = product $ largestThree $ extend basins
  where
    minima = findMinima floor
    basins = foldr (Map.insert <*> Basin) (Map.map initFloor floor) minima

    initFloor 9 = Wall
    initFloor _ = Unknown

    extend :: Map (Int, Int) Floor -> Map (Int, Int) Floor
    extend basins
      | Map.null (Map.filter (== Unknown) basins) = basins
      | otherwise = extend (Map.mapWithKey propagate basins)
      where
        propagate :: (Int, Int) -> Floor -> Floor
        propagate coord Unknown = case filter isBasin $ map (basins !) $ findNeighbors basins coord of
          [] -> Unknown
          (basin : _) -> basin
        propagate _ floor = floor

    largestThree =
      take 3 . reverse . L.sort
        . Map.elems
        . Map.fromListWith (+)
        . flip zip (repeat 1)
        . Map.elems
        . Map.filter isBasin
