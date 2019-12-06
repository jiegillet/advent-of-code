import Data.Array

power :: Int -> [[Int]]
power serial = map (\x -> map (pow x) [1..300]) [1..300]
  where
    pow x y = mod (div (rack * (serial + rack * y)) 100) 10 - 5
      where rack = x + 10

shiftX :: Int -> [[Int]] -> [[Int]]
shiftX n = map (drop n)

shiftY :: Int -> [[Int]] -> [[Int]]
shiftY n = drop n

add :: [[Int]] -> [[Int]] -> [[Int]]
add = zipWith (zipWith (+))

largestPow :: Int -> (Int, (Int, Int))
largestPow serial = maxPow
  where
    mat = power serial
    conv = foldr1 add [shiftX x $ shiftY y mat | x <- [0..2], y <- [0..2]]
    indices = [[(j, i) | i <- [1..300]] | j <- [1..300]]
    maxPow = maximum $ concat $ zipWith (zip) conv indices

largestLargestPow :: Int -> (Int, (Int, Int, Int))
largestLargestPow serial = maxPow
  where
    mat = power serial
    cumMat = scanl (zipWith (+)) (replicate 301 0) . map (scanl (+) 0) $ mat
    cumArray = listArray ((0,0),(300,300)) $ concat cumMat
    cumPow x y size = cumArray!(x-1+size,y-1+size)
                    - cumArray!(x-1,y-1+size)
                    - cumArray!(x-1+size,y-1)
                    + cumArray!(x-1,y-1)
    maxPow = maximum [(cumPow x y size, (x, y, size)) | x <- [1..300]
                                                      , y <- [1..300]
                                                      , size <- [1..300-max x y]]

main = do
  print $ largestPow 7403
  print $ largestLargestPow 7403
