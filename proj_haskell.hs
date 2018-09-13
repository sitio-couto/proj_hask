import Data.List.Split

main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      waitTime = getBusData $ contents!!1
      graph = reduce (mergePaths waitTime $ buildGraph waitTime (contents!!0))
      (start:[finish]) = splitOn " " $ head (contents!!2)
      in
        print graph

getBusData [] = []
getBusData (x:xs) = (a,(read b::Float)/2):getBusData xs
  where (a:[b]) = splitOn " " x

buildGraph _ [] = []
buildGraph b (x:xs) = addEdge n (v,t,total_w) (buildGraph b xs)
  where
    total_w = foldl (\c (l,wt) -> if l == t then c+wt else c) (read w::Float) b
    (n:v:t:[w]) = splitOn " " x

reduce [] = []
reduce ((v,e),gs) = (v,getShort e e):(reduce gs)
getShort ((v,t,w):es) acc
  | (remove e es)
  where
    survivor = getMin kill
    kill = filter (\(n,_,_) -> v == n) acc

getMin (k:ks) = foldl (\c x -> if test c x then c else x ) k ks
  where test (v1,t1,w1) (v2,t2,w2) = w1 < w1

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

-- mergeBusPaths Checked
mergePaths [] g = []
mergePaths ((l,_):bs) g = foldVertex (mergeBusPaths bs g) g l

-- TODO NOT Checked
foldVertex [] g _ = g
foldVertex (gi:gs) g b = addPaths gi (foldVertex gs g b) b

--addPaths checked
addPaths (v,e) g b
  | (length paths < 2) = g
  | otherwise = foldr (\x acc -> addEdge v x acc) g $ combEdges paths
  where paths = tracePaths g e b [] [v]

-- tracePaths checked
tracePaths g pe b p visited
  | test = tracePaths g (getE v g) b (p++next) (v:visited)
  | otherwise = p
  where
    test = (next /= [])&&(not $ elem v visited)
    (v,_,_) = head next
    next = filter (\(_,mode,_) -> mode == b) pe

-- combEdges Checked
combEdges ((v,t,w):p) = combEdges' (tail p) [(joinEdges e0 $ head p)]
  where e0 = (v,t,w)

-- combEdges' Checked
combEdges' [] acc = acc
combEdges' p acc = combEdges' (tail p) $ (joinEdges (head acc) $ head p):acc

-- joinEdges Checked
joinEdges (ov,ot,ow) (v,t,w) = (v,ot++" "++ov++" "++t,nw)
  where nw = (fromIntegral $ floor ((ow+w)*10))/10

-- GetE checked
getE target g = edges
  where (_,edges) = head $ filter (\(v,e) -> v == target) g
