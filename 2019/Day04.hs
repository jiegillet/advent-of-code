module Day04 where

import Data.List (group)

main :: IO ()
main = do
  let low = 172930
      high = 683082
  print $ length [i | i <- [low..high], increase i, adjacent i]
  print $ length [i | i <- [low..high], increase i, adjacent2 i]

adjacent :: Integer -> Bool
adjacent = not . null .  filter ((>1) .length)  . group . show

adjacent2 :: Integer -> Bool
adjacent2 = not . null .  filter ((==2) .length)  . group . show

increase :: Integer -> Bool
increase = all id . (zipWith (<=) <*> tail)  . show
