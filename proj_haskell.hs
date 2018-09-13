
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

-- mergeBusPaths Checked
mergePaths [] g = []
mergePaths (b:bs) g = foldVertex (mergeBusPaths bs g) g b

-- TODO NOT Checked
foldVertex [] g _ = g
foldVertex (gi:gs) g b = addPaths gi (foldVertex gs g b) b

--TODO Fixing initial sum issue
addPaths (v,e) g (l,w)
  | (length paths < 2) = g
  | otherwise = foldr (\x acc -> addEdge v x acc) g $ combEdges paths w
  where paths = tracePaths g e l [] [v]

-- tracePaths checked
tracePaths g pe l p visited
  | test = tracePaths g (getE v g) l (p++next) (v:visited)
  | otherwise = p
  where
    test = (next /= [])&&(not $ elem v visited)
    (v,_,_) = head next
    next = filter (\(_,mode,_) -> mode == l) pe

-- TODO Fixing initial sum issue
combEdges (p:ps) w = combEdges' w (tail ps) [(joinEdges p $ head ps)]

-- TODO Fixing initial sum issue
combEdges' [] acc = acc
combEdges' (p:ps) acc = combEdges' w ps $ joinEdges (head acc) ((v,t,wt):acc)
  where wt = w0 - w
        (v,t,w0) = p

-- joinEdges Checked
joinEdges (ov,ot,ow) (v,t,w) = (v,ot++" "++ov++" "++t,nw)
  where nw = (fromIntegral $ floor ((ow+w)*10))/10

-- GetE checked
getE target g = edges
  where (_,edges) = head $ filter (\(v,e) -> v == target) g
