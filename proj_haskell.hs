import Data.List.Split

main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      bus = getBusData $ contents!!1
      graph = buildGraph $ contents!!0
      (start:[finish]) = splitOn " " $ head (contents!!2)
      in
        print graph

getBusData [] = []
getBusData (x:xs) = (a,(read b::Float)/2):getBusData xs
  where (a:[b]) = splitOn " " x

buildGraph [] = []
buildGraph (x:xs) = addEdge n (v,t,read w::Float) (buildGraph xs)
  where (n:v:t:[w]) = splitOn " " x

addEdge node link [] = [(node,[link])]
addEdge node link ((v,es):graph)
  | (node == v) = (v,(link:es)):graph
  | otherwise = (v,es):addEdge node link graph

splitData l = splitData' l []
splitData' :: [String] -> [String] -> [[String]]
splitData' [] acc = [acc]
splitData' (x:xs) acc
  | (x /= "") = splitData' xs $ x:acc
  | otherwise = (reverse acc):splitData' xs []

-- TODO NOT Checked
foldVertex [] g _ = g
foldVertex (gi:gs) g b = addPaths gi (foldVertex gs g b) b

--addPaths checked
addPaths (v,e) g (l,wt)
  | (length paths < 2) = g
  | otherwise = foldr (\x acc -> addEdge v x acc) g $ combEdges paths wt
  where paths = tracePaths g e l [] [v]

-- tracePaths checked
tracePaths g pe b p visited
  | test = tracePaths g (getE v g) b (p++next) (v:visited)
  | otherwise = p
  where
    test = (next /= [])&&(not $ elem v visited)
    (v,_,_) = head next
    next = filter (\(_,mode,_) -> mode == b) pe

-- combEdges Checked
combEdges ((v,t,w):p) wt = combEdges' (tail p) [(joinEdges e0 $ head p)]
  where e0 = (v,t,w+wt)

-- combEdges' Checked
combEdges' [] acc = acc
combEdges' p acc = combEdges' (tail p) $ (joinEdges (head acc) $ head p):acc

-- joinEdges Checked
joinEdges (ov,ot,ow) (v,t,w) = (v,ot++" "++ov++" "++t,nw)
  where nw = (fromIntegral $ floor ((ow+w)*10))/10

-- GetE checked
getE target g = edges
  where (_,edges) = head $ filter (\(v,e) -> v == target) g
