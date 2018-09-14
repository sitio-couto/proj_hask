
main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      waitTime = getBusData $ contents!!1
      graph = reduce (mergePaths waitTime $ buildGraph waitTime (contents!!0))
      (start:[finish]) =  words $ head (contents!!2)
      in
        print graph

getBusData [] = []
getBusData (x:xs) = (a,(read b::Float)/2):getBusData xs
  where (a:[b]) = words x

buildGraph _ [] = []
buildGraph b (x:xs) = addEdge n (v,t,total_w) (buildGraph b xs)
  where
    total_w = foldl (\c (l,wt) -> if l == t then c+wt else c) (read w::Float) b
    (n:v:t:[w]) = words x

--TODO reduce from multi to simple graph
reduce [] = []
reduce ((v,e),gs) = (v,getShort e):(reduce gs)

--TODO reduce from  multi to simple graph
getShort e = foldr (\(n,_,_) c -> (getMin $ filter (\(n,_,_) -> v == n)):c) [] e

--TODO reduce from mutli to simple graph
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

-- TODO DOESNT ITERATE CORRECTLY
mergePaths [] g = g
mergePaths (b:bs) g = foldVertex (mergePaths bs g) g b

-- foldVertex Checked
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
combEdges (p:ps) wt = combEdges' wt (tail ps) [(joinEdges wt p $ head ps)]

-- combEdges' Checked
combEdges' _ [] acc = acc
combEdges' wt p acc = combEdges' wt (tail p) $ (joinEdges wt (head acc) $ head p):acc

-- joinEdges Checked
joinEdges w (ov,ot,ow) (nv,nt,nw) = (nv,ot++" "++ov++" "++nt,tw)
  where tw = (fromIntegral $ floor ((ow+nw-w)*10))/10

-- GetE checked
getE target g = edges
  where (_,edges) = head $ filter (\(v,e) -> v == target) g
