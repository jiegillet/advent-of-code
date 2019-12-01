import Data.Char (ord)
import Data.List (insert, nub, sort, (\\))
import qualified Data.Map as M

main :: IO ()
main = do
  txt <- readFile "../input/Day7.txt"
  putStrLn $ uncurry singleBuild $ prep txt
  mapM_ print $ uncurry parallelBuild $ prep txt

prep :: String -> (M.Map Char String, M.Map Char String)
prep txt = (to, from)
  where
    to = M.fromListWith (++) $ map extractTo $ lines txt
    from = M.fromListWith (++) $ map extractFrom $ lines txt
    extractTo s = (s!!5, [s!!36])
    extractFrom s = (s!!36, [s!!5])

singleBuild :: M.Map Char String -> M.Map Char String -> String
singleBuild to from =  build to from (next to from)
  where
    next to from = head' $ filter (not . flip M.member from) $ M.keys to
    end = filter (not . flip M.member to) $ M.keys from

    build _ _ Nothing = end
    build to from (Just c) = c : build to' from' (next to' from')
      where
        to' = M.delete c to
        from' = M.filter (not . null) $ M.map (filter (/= c)) from

    head' (a:_) = Just a
    head' []    = Nothing

-- parallelBuild :: M.Map Char String -> M.Map Char String -> String
parallelBuild to from = build to from [] (next to from)
  where
    next to from = filter (not . flip M.member from) $ M.keys to
    end = filter (not . flip M.member to) $ M.keys from

--    build to from [] [] = end
    build to from workers todo =
        let open = 5 - length workers
            workers' = foldr chInsert workers (take open todo)
            (done, rest) = pop workers'
            to' = foldr M.delete to done
            from' = foldr (\c -> M.filter (not . null) . M.map (filter (/= c))) from done
            todo' = next to' from' \\ map snd rest
        in (done, rest, todo') : build to' from' rest todo'

    pop a@((s, _):as) =
      let (pops, rest) = span (\(s', _) -> s == s') a
      in (map snd pops, map (\(s', x) -> (s'-s, x)) rest)

    chInsert c = insert (ord c - 4, c)


ex = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."
