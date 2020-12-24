module Main where

import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- lines <$> readFile "Day24.txt"
  let paths = map toPath input
  --   print $ part1 $ map toPath ex
  --   print $ part2 $ map toPath ex
  print $ part1 paths
  print $ part2 paths

data Coord a = Coord a a deriving (Show, Eq, Ord)

instance (Semigroup a) => Semigroup (Coord a) where
  (Coord a b) <> (Coord x y) = Coord (a <> x) (b <> y)

instance (Monoid a) => Monoid (Coord a) where
  mempty = Coord mempty mempty

instance Functor Coord where
  fmap f (Coord a b) = Coord (f a) (f b)

toPath :: String -> [Coord Int]
toPath "" = []
toPath ('e' : rest) = Coord 0 1 : toPath rest
toPath ('w' : rest) = Coord 0 (-1) : toPath rest
toPath ('n' : 'e' : rest) = Coord 1 0 : toPath rest
toPath ('n' : 'w' : rest) = Coord 1 (-1) : toPath rest
toPath ('s' : 'e' : rest) = Coord (-1) 1 : toPath rest
toPath ('s' : 'w' : rest) = Coord (-1) 0 : toPath rest

initialState :: [[Coord Int]] -> Set (Coord Int)
initialState = foldr (switch . toCoord) Set.empty
  where
    toCoord = fmap getSum . foldMap (fmap Sum)
    switch coord grid =
      if Set.member coord grid
        then Set.delete coord grid
        else Set.insert coord grid

part1 :: [[Coord Int]] -> Int
part1 = Set.size . initialState

part2 :: [[Coord Int]] -> Int
part2 = Set.size . (!! 100) . iterate step . initialState
  where
    step :: Set (Coord Int) -> Set (Coord Int)
    step grid = foldMap evolve $ foldMap neighbors grid
      where
        evolve :: Coord Int -> Set (Coord Int)
        evolve pos
          | Set.member pos grid, c == 1 || c == 2 = Set.singleton pos
          | not $ Set.member pos grid, c == 2 = Set.singleton pos
          | otherwise = Set.empty
          where
            c = Set.size $ Set.intersection (neighbors pos) grid

    neighbors :: Coord Int -> Set (Coord Int)
    neighbors (Coord i j) =
      Set.fromAscList
        [ Coord (i -1) j,
          Coord (i -1) (j + 1),
          Coord i (j -1),
          Coord i (j + 1),
          Coord (i + 1) (j -1),
          Coord (i + 1) j
        ]

ex :: [String]
ex =
  [ "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew"
  ]