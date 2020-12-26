module Main where

import Data.Bits

main :: IO ()
main = do
  let card = 8184785
      door = 5293040
      prime = 20201227
  print $ part1 card door prime

part1 :: Integer -> Integer -> Integer -> Integer
part1 card door prime = modExp door nCard prime
  where
    nCard = head $ filter (\n -> modExp 7 n prime == card) [1 ..]

-- Stolen from https://gist.github.com/trevordixon/6788535
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where
    t = if testBit e 0 then b `mod` m else 1
