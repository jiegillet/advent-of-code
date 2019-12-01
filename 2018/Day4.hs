import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (foldl', isInfixOf, maximumBy, sort, sortBy, splitAt)

ex = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

schedule :: String -> IntMap (IntMap Int)
schedule = foldr getTimes M.empty . group . sort . lines
 where
   group :: [String] -> [[String]]
   group [] = []
   group (l:ls) =
     let (i, o) = span (not . isInfixOf "Guard") ls
     in (l: i) : group o

   getTimes :: [String] -> IntMap (IntMap Int) -> IntMap (IntMap Int)
   getTimes (grd: events) = M.insertWith (M.unionWith (+)) (getID grd) times
     where
       times :: IntMap Int
       times = foldl' time (M.fromList $ zip [0..59] (repeat 0)) events

       time :: IntMap Int -> String -> IntMap Int
       time hour line
         | isInfixOf "sleep" line =
           M.mapWithKey (\m -> if m < getMinute line then id else succ) hour
         | isInfixOf "wakes" line =
           M.mapWithKey (\m -> if m < getMinute line then id else pred) hour

       getID :: String -> Int
       getID = read . takeWhile (/=' ') . drop 26

       getMinute :: String -> Int
       getMinute = read . takeWhile (/=']') . drop 15

findMax :: Ord a => IntMap a -> (M.Key, a)
findMax = maximumBy (compare `on` snd) . M.toList

sneak :: String -> Int
sneak txt = guard * minute
 where
   s = schedule txt
   guard = bestGuard s
   minute = bestMinute guard s
   bestGuard = fst . findMax . M.map (sum . map snd . M.toList)
   bestMinute guard = fst . findMax . (M.! guard)


sleepyMinute :: String -> Int
sleepyMinute txt = guard * minute
 where
   s = schedule txt
   (guard, (_, minute)) = findMax $ M.map (swap . findMax) s
   swap (x, y) = (y, x)

main = do
 txt <- readFile "../input/Day4.txt"
 print $ sneak txt
 print $ sleepyMinute txt
