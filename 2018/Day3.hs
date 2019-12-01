import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (groupBy, nub, sort)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec

ex = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

data Rectangle = Rectangle
 { _id     :: Int
 , _left   :: Int
 , _top    :: Int
 , _right  :: Int
 , _bottom :: Int
 } deriving (Show, Eq)

intP :: Parsec String () Int
intP = read <$> (skipMany space *> many1 digit <* skipMany space)

rectangleP :: Parsec String () Rectangle
rectangleP = do
 i <- char '#' *> intP
 left <- char '@' *> intP
 top <- char ',' *> intP <* char ':'
 width <- intP
 height <- char 'x' *> intP
 return $ Rectangle i left top (left + width - 1) (top + height - 1)

parseP :: Parsec String () [Rectangle]
parseP = many rectangleP

data SegTree
 = Leaf Int (Set Rectangle)
 | Node  Int Int SegTree SegTree (Set Rectangle) deriving (Show, Eq)

left, right :: SegTree -> Int
left (Leaf a _)       = a
left (Node a _ _ _ _) = a
right (Leaf a _)       = a
right (Node _ a _ _ _) = a

rect :: SegTree -> Set Rectangle
rect (Leaf _ a)       = a
rect (Node _ _ _ _ a) = a

instance Ord Rectangle where
  compare = compare `on` _left

instance Ord SegTree where
  compare = compare `on` left

slices :: [Rectangle] -> IntMap [Rectangle]
slices rects = foldr add (M.fromList $ zip extrema (repeat [])) rects
  where
    extrema = nub $ sort $ concatMap (\r -> [_top r, _bottom r]) rects
    add r@(Rectangle _ _ t _ b) m =
      foldr (\i m -> M.insertWith (++) i [r] m) m [t..b]

buildHorizontal :: [Rectangle] -> SegTree
buildHorizontal r = foldr insert (makeTree r) r
 where
   makeTree :: [Rectangle] -> SegTree
   makeTree = head . head . dropWhile ((>1) . length) . iterate go . leaves
   leaves :: [Rectangle] -> [SegTree]
   leaves = map (\i -> Leaf i S.empty) .
           nub . sort . concatMap (\r -> [_left r, _right r])
   go (a:b:rest) =
     Node (left a) (right b) a b (S.intersection (rect a) (rect b)) : go rest
   go x          = x
   insert rect@(Rectangle _ lr _ rr _) n@(Node l r ln rn s)
     | lr <= l && rr >= r = Node l r ln rn (S.insert rect s)
     | lr > r = n
     | rr < l = n
     | otherwise = Node l r (insert rect ln) (insert rect rn) s
   insert rect@(Rectangle _ lr _ rr _) n@(Leaf i s)
     | i == lr || i == rr = Leaf i (S.insert rect s)
     | otherwise = n

overlap :: SegTree -> Int
overlap (Leaf _ rects) = if S.size rects > 1 then 1 else 0
overlap (Node l r ln rn rects)
  | S.null rects = overlap ln + overlap rn
  | S.size rects > 1 = r - l + 1
  | otherwise = below ln + below rn
  where below (Leaf _ rects)
          | S.null rects = 0
          | otherwise = 1
        below (Node l r ln rn rects)
          | S.null rects = below ln + below rn
          | otherwise = r - l + 1

sol1 :: String -> Int
sol1 s =
  let Right rects = parse parseP "" s
      trees = M.toList $ M.map (overlap . buildHorizontal) $ slices rects
      total = sum $ zipWith (\(i, t) (j,_) -> (j-i)*t) trees (tail trees)
  in total

main = do
  txt <- readFile "../input/Day3.txt"
  print $ sol1 txt

-- Node 1 6
--   (Node 1 5
--     (Node 1 3
--       (Leaf 1 (fromList []))
--       (Leaf 3 (fromList [Rectangle {_id = 2, _left = 3, _top = 1, _right = 6, _down = 4}]))
--       (fromList [Rectangle {_id = 1, _left = 1, _top = 3, _right = 4, _down = 6}]))
--     (Node 4 5
--       (Leaf 4 (fromList [Rectangle {_id = 1, _left = 1, _top = 3, _right = 4, _down = 6}]))
--       (Leaf 5 (fromList [Rectangle {_id = 3, _left = 5, _top = 5, _right = 6, _down = 6}]))
--       (fromList [Rectangle {_id = 2, _left = 3, _top = 1, _right = 6, _down = 4}]))
--     (fromList []))
--   (Leaf 6 (fromList [Rectangle {_id = 2, _left = 3, _top = 1, _right = 6, _down = 4},Rectangle {_id = 3, _left = 5, _top = 5, _right = 6, _down = 6}]))
--   (fromList []) -- overlap 1

-- 1 2 3 4 5 6
-- # # # #
--     # # # #
--         # #
