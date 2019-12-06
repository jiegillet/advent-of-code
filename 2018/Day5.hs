import Data.List (foldl', minimumBy)
import Data.Char (toUpper, toLower, isUpper,isLower, isAlpha)
import Data.Function (on)

import qualified Data.Set as S

ex = "dabAcCaCBAcCcaDA"


reduce :: String -> String
reduce = reverse . foldl' go "" . filter isAlpha
  where
    go [] c = [c]
    go (r:rs) c
      | react r c = rs
      | otherwise = c: r: rs
    react r c = toLower r == toLower c
                && any isLower [r, c]
                && any isUpper [r, c]

improve :: String -> String
improve s = reduce $ minimumBy (compare `on` length . reduce) $ map remove types
  where
    types = S.toList $ S.fromList $ map toLower s
    remove c = filter (`notElem` [c, toUpper c]) s

main = do
  txt <- readFile "Day4.txt"
  print $ length txt
  print $ length $ reduce txt
  print $ length $ improve txt
