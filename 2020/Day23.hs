{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.IntMap ((!))
import qualified Data.IntMap as Map
import Data.List (elemIndex, (\\))
import Debug.Trace

main :: IO ()
main = do
  let input = [8, 7, 2, 4, 9, 5, 1, 3, 6]
  -- let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
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
part2 = format . fst . (!! 10_000_000) . iterate play . initial . fill
  where
    fill = (++ [10 .. 1_000_000])
    initial l = (Map.fromList $ zip l (tail $ cycle l), head l)
    play (cups, current) = (cups', cups' ! current)
      where
        backTo9 n = if n == 0 then 1_000_000 else n
        removed@[a, b, c] = take 3 $ tail $ iterate (cups !) current
        destination = head $ filter (not . (`elem` removed)) $ tail $ iterate (backTo9 . subtract 1) current
        cups' =
          Map.insert current (cups ! c) $
            Map.insert c (cups ! destination) $
              Map.insert destination a cups
    format cups = product $ take 2 $ tail $ iterate (cups !) 1
