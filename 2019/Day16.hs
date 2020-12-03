module Day01 where

main :: IO ()
main = do
  signal <- map (read . pure) . init <$> readFile "Day16.txt"
  -- let signal = map (read . pure) "80871224585914546619083218645595"
  putStrLn $ part1 signal
  putStrLn $ part1' signal
  -- putStrLn $ part2 signal



part1 :: [Integer] -> String
part1 signal = concatMap show $ take 8 $ (!!100) $ iterate fft signal
  where
    base = [0, 1, 0, -1]

    fft :: [Integer] -> [Integer]
    fft s = map (output s) [1..length signal]

    output :: [Integer] -> Int -> Integer
    output s n = cut $ sum $ zipWith (*) expandedBase s
      where
        expandedBase = tail $ cycle $ base >>= replicate n
        cut k = mod (abs k) 10


part1' :: [Integer] -> String
part1' signal = concatMap show $ map (`mod` 10) $ take 8 $ (!!100) $ iterate fft signal
  where
    base = [0, 1, 0, -1]

    fft :: [Integer] -> [Integer]
    fft s = map (output s) [1..length signal]

    output :: [Integer] -> Int -> Integer
    output s n = cut $ sum $ zipWith (*) expandedBase s
      where
        expandedBase = tail $ cycle $ base >>= replicate n
        cut k = abs k


part2 :: [Integer] -> String
part2 signal' = concatMap show $ take 8 $ drop offset $ (!!100) $ iterate fft signal
  where
    signal = concat $ replicate 10000 signal'
    base = [0, 1, 0, -1]
    offset = read $ concatMap show $ take 7 signal

    fft :: [Integer] -> [Integer]
    fft s = map (output s) [1..length signal]

    output :: [Integer] -> Int -> Integer
    output s n = cut $ sum $ zipWith (*) expandedBase s
      where
        expandedBase = tail $ cycle $ base >>= replicate n
        cut k = mod (abs k) 10

-- part2 :: [Int] -> Int
-- part2 = sum . map (sum . tail . takeWhile (>0) . iterate fuel)
