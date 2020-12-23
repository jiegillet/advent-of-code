{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as Map
import Data.List (elemIndex, (\\))
import Debug.Trace

main :: IO ()
main = do
  --   let input = [8, 7, 2, 4, 9, 5, 1, 3, 6]
  let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
  print $ part1 input
  print $ part2 input

part1 :: [Int] -> String
part1 = format . fst . (!! 100) . iterate play . (\l -> (l, head l))
  where
    play (cups, current) = (cups', current')
      where
        backTo9 n = if n == 0 then 9 else n
        removed = take 3 $ tail $ dropWhile (/= current) $ cycle cups
        remaining = cups \\ removed
        destination = head $ filter (`elem` remaining) $ tail $ iterate (backTo9 . subtract 1) current
        (prev, _ : post) = span (/= destination) remaining
        cups' = prev ++ destination : removed ++ post
        current' = head $ tail $ dropWhile (/= current) $ cycle cups'

    format = concatMap show . takeWhile (/= 1) . tail . dropWhile (/= 1) . cycle

part2 :: [Int] -> Int
part2 cups = format $ undefined $ (!! 10_000_000) $ iterate play (cupToPos, posToCup, head cups)
  where
    cupToPos = Map.union (Map.fromList $ zip cups [0 ..]) (Map.fromAscList $ zip [9 ..] [10 .. 1_000_000])
    posToCup = Map.union (Map.fromList $ zip [0 ..] cups) (Map.fromAscList $ zip [10 .. 1_000_000] [9 ..])
    play (cupToPos, posToCup, current) = undefined
      where
        cupMod n = if n == 0 then 1_000_000 else n
        posMod n = if n >= 1_000_000 then n - 1_000_000 else n
        removed = let pos = cupToPos ! current in map ((posToCup !) . posMod) [pos + 1, pos + 2, pos + 3]
        destination = head $ filter (not . (`elem` removed)) $ tail $ iterate (cupMod . subtract 1) current
        destinationPos = cupToPos ! destination

    format = product . take 2 . tail . dropWhile (/= 1) . cycle
