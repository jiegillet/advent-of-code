module Day01 where

import Data.List
import Data.Function (on)

main :: IO ()
main = do
  pixels <- init <$> readFile "Day08.txt"

  let height = 6
      width = 25
  print $ part1 width height pixels
  putStrLn $ part2 width height pixels

part1 :: Int -> Int -> [Char] -> Int
part1 width height pixels =
    let layers = chunks (height * width) pixels
        layer = minimumBy (compare `on` length . filter (=='0')) layers
        ones = length $ filter (=='1') layer
        twos = length $ filter (=='2') layer
    in ones * twos

part2 :: Int -> Int -> [Char] -> String
part2 width height = unlines . chunks width . foldl1 fuse . chunks (height * width)
  where
    fuse = zipWith priority

    priority '2' '1' = 'X'
    priority '2' '0' = ' '
    priority '2' c = c
    priority '0' _ = ' '
    priority '1' _ = 'X'
    priority c   _ = c

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l = let (a, b) = splitAt n l in a : chunks n b
