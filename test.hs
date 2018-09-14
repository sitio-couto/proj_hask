
path = [("b","a-pe",0.4),("d","linha-370",0.1),("f","a-pe",3.0),("c","a-pe",0.3)]
g0 = [("a",[("b","a-pe",0.4),("d","linha-370",0.1)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)]),("c",[("b","a-pe",0.5),("d","a-pe",0.3)])]
g1 = [("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("f",[("h","a-pe",12.3),("h","linha-567",1.2)]),("h",[])]
g00 = [("f",[("h","linha-567",7.2),("h","a-pe",12.3)]),("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("a",[("b","a-pe",0.4),("d","linha-370",7.6)])]
g11 = [("c",[("b","a-pe",0.5),("d","a-pe",0.3)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)])]
g3 = [("a",[("b","linha-666",0.5)]),("b",[("c","linha-666",0.2)]),("c",[("d","linha-666",0.1)]),("d",[])]
g =g0++g1

main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      waitTime = getBusData $ contents!!1
      graph = buildGraph waitTime (contents!!0)
      procGraph = mergePaths waitTime graph
      (start:[finish]) =  words $ head (contents!!2)
      in
        print (graph == g)

getBusData [] = []
getBusData (x:xs) = (a,(read b::Float)/2):getBusData xs
  where (a:[b]) = words x

buildGraph _ [] = []
buildGraph b (x:xs) = addEdge n (v,t,total_w) (buildGraph b xs)
  where
    total_w = foldl (\c (l,wt) -> if l == t then c+wt else c) (read w::Float) b
    (n:v:t:[w]) = words x

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

--------------------------------------------------------------------------------

-- mergeBusPaths Checked
mergePaths [] g = g
mergePaths (b:bs) g = foldVertex (mergePaths bs g) g b

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
