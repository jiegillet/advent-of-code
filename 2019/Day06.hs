module Day06 where

import           Data.Graph
import qualified Data.Map

main :: IO ()
main = do
  orbits <- map parseOrbit . lines <$> readFile "Day06.txt"
  print $ part1 orbits
  print $ part2 orbits

parseOrbit :: String -> (String, String)
parseOrbit s = (a, tail b')
  where
    (a, b') = break (==')') s

part1 ::  [(String, String)] -> Int
part1 o = expand 0 "COM"
  where
    m = Data.Map.fromListWith (++) $ map (fmap pure) o
    expand checksum obj = case Data.Map.lookup obj m of
      Nothing   -> checksum
      Just obj' -> checksum + sum (map (expand (checksum + 1)) obj')

part2 ::  [(String, String)] -> Int
part2 o = length youPath + length sanPath - 2 * overlap
  where
    youPath = reverse $ orbitUp "YOU"
    sanPath = reverse $ orbitUp "SAN"
    overlap = length $ takeWhile (==True) $ zipWith (==) youPath sanPath
    swap (a, b) = (b, a)
    m = Data.Map.fromList $ map swap o
    orbitUp obj = case Data.Map.lookup obj m of
      Nothing   -> []
      Just obj' -> obj' : orbitUp obj'

test = [("COM", "B") , ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L"), ("K", "YOU"), ("I", "SAN")]
