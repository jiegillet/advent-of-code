module Main where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "Day20.txt"
  let tiles = toTiles input
  print $ part1 tiles
  print $ part2 tiles

data Tile = Tile Int [[Char]] deriving (Show)

getId :: Tile -> Int
getId (Tile id _) = id

getGrid :: Tile -> [[Char]]
getGrid (Tile _ grid) = grid

toTiles :: String -> [Tile]
toTiles = toTile . lines
  where
    toTile :: [String] -> [Tile]
    toTile [] = []
    toTile (('T' : 'i' : 'l' : 'e' : ' ' : id) : tiles) = Tile (read $ init id) tile : toTile (drop (size + 1) tiles)
      where
        size = length $ head tiles
        tile = take size tiles

part1 :: [Tile] -> Int
part1 tiles = product $ map getId $ filter isCorner tiles
  where
    isCorner = (== [1, 1, 2, 2]) . sort . map (\edge -> length $ filter (== edge) (concat allEdges)) . edges
    allEdges = map edges tiles
    edges (Tile _ tile) =
      map
        (\toEdge -> min (toEdge tile) (reverse $ toEdge tile))
        [head, last, map head, map last]

data Piece = Piece
  { left :: String,
    right :: String,
    up :: String,
    down :: String,
    piece :: Array (Int, Int) Char,
    tile :: Int
  }
  deriving (Show, Eq)

rotate :: Array (Int, Int) a -> Array (Int, Int) a
rotate a = let (_, (_, n)) = A.bounds a in A.ixmap (A.bounds a) (\(i, j) -> (n - j, i)) a

flipA :: Array (Int, Int) a -> Array (Int, Int) a
flipA a = A.ixmap (A.bounds a) (\(i, j) -> (j, i)) a

reconstruct :: [Tile] -> Array (Int, Int) Char
reconstruct tiles = image
  where
    corner = head $ filter cornerPiece pieces
    cornerPiece (Piece left _ up _ _ _) = length (piecesLeft Map.! left ++ piecesUp Map.! up) == 2

    tileSize = length (getGrid $ head tiles) - 2
    size = floor (sqrt $ fromIntegral $ length tiles)
    row0 = take size $ iterate findRightMatch corner
    grid = take size $ iterate (map findDownMatch) row0
    findDownMatch piece = head $ filter (\p -> tile piece /= tile p) $ piecesUp Map.! down piece
    findRightMatch piece = head $ filter (\p -> tile piece /= tile p) $ piecesLeft Map.! right piece

    image =
      A.array
        ((0, 0), (size * tileSize -1, size * tileSize -1))
        [ (((size - i - 1) * tileSize + k, j * tileSize + l), a ! (k, l))
          | i <- [size -1, size -2 .. 0],
            j <- [0 .. size -1],
            let a = piece $ (grid !! i) !! j,
            k <- [0 .. tileSize -1],
            l <- [0 .. tileSize -1]
        ]

    pieces :: [Piece]
    pieces = concatMap allOrientations tiles

    piecesLeft, piecesUp :: Map String [Piece]
    piecesLeft = Map.fromListWith (++) $ map (\p -> (left p, [p])) pieces
    piecesUp = Map.fromListWith (++) $ map (\p -> (up p, [p])) pieces

    allOrientations :: Tile -> [Piece]
    allOrientations (Tile id tile) = map toPiece $ scanr ($) array transformation
      where
        n = length tile - 1
        array = A.listArray ((0, 0), (n, n)) (concat tile)
        transformation = [rotate, rotate, rotate, flipA, rotate, rotate, rotate]

        toPiece a =
          Piece
            { left = [a ! (i, 0) | i <- [0 .. n]],
              right = [a ! (i, n) | i <- [0 .. n]],
              up = [a ! (n, j) | j <- [0 .. n]],
              down = [a ! (0, j) | j <- [0 .. n]],
              piece = A.array ((0, 0), (n -2, n -2)) [((i, j), a ! (i + 1, j + 1)) | i <- [0 .. n -2], j <- [0 .. n -2]],
              tile = id
            }

part2 :: [Tile] -> Int
part2 tiles = roughness
  where
    image = reconstruct tiles
    ((0, 0), (_, size)) = A.bounds image
    images = scanr ($) image [rotate, rotate, rotate, flipA, rotate, rotate, rotate]
    findMonster image i j = all ((== '#') . (\(k, l) -> image ! (i + k, j + l))) monster
    monster = [(0, 1), (0, 4), (0, 7), (0, 10), (0, 13), (0, 16), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19), (2, 18)]
    monsters = length [() | i <- [0 .. size -2], j <- [0 .. size -20], im <- images, findMonster im i j]
    roughness =
      length [() | i <- [0 .. size], j <- [0 .. size], image ! (i, j) == '#']
        - monsters * length monster

printArray :: Array (Int, Int) Char -> String
printArray a = unlines [[a ! (i, j) | j <- [j0 .. jN]] | i <- [iN, iN -1 .. i0]]
  where
    ((i0, j0), (iN, jN)) = A.bounds a

exImage :: Array (Int, Int) Char
exImage =
  A.listArray ((0, 0), (23, 23)) $
    concat $
      reverse
        [ ".#.#..#.##...#.##..#####",
          "###....#.#....#..#......",
          "##.##.###.#.#..######...",
          "###.#####...#.#####.#..#",
          "##.#....#.##.####...#.##",
          "...########.#....#####.#",
          "....#..#...##..#.#.###..",
          ".####...#..#.....#......",
          "#..#.##..#..###.#.##....",
          "#.####..#.####.#.#.###..",
          "###.#.#...#.######.#..##",
          "#.####....##..########.#",
          "##..##.#...#...#.#.#.#..",
          "...#..#..#.#.##..###.###",
          ".#.#....#.##.#...###.##.",
          "###.#...#..#.##.######..",
          ".#.#.###.##.##.#..#.##..",
          ".####.###.#...###.#..#.#",
          "..#.#..#..#.#.#.####.###",
          "#..####...#.#.#.###.###.",
          "#####..#####...###....##",
          "#.##..#..#...#..####...#",
          ".#.###..##..##..####.##.",
          "...###...##...#...#..###"
        ]