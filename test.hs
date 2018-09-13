import Data.List.Split
data Edge  a = Edge (a, String, Float) deriving (Show, Read, Eq)
data Node  a = Node (a,[Edge a]) deriving (Show, Read, Eq)
data Bus   a = Bus (String, Float)

path = [("b","a-pe",0.4),("d","linha-370",0.1),("f","a-pe",3.0),("c","a-pe",0.3)]
g0 = [("a",[("b","a-pe",0.4),("d","linha-370",0.1)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)]),("c",[("b","a-pe",0.5),("d","a-pe",0.3)])]
g1 = [("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("f",[("h","a-pe",12.3),("h","linha-567",1.2)]),("h",[])]
g3 = [("a",[("b","linha-666",0.5)]),("b",[("c","linha-666",0.2)]),("c",[("d","linha-666",0.1)]),("d",[])]
g =g3

addEdge node link ((v,es):graph)
  | (node == v) = (v,(link:es)):graph
  | otherwise = (v,es):addEdge node link graph

-- mergeBusPaths :: [Node a] -> [Bus b] -> [Node a]
mergeBusPaths g [] = g
mergeBusPaths g (b:bs) = foldVertex (mergeBusPaths g bs) g b

-- foldVertex :: [Node a] -> [Node a] -> Bus b -> [Node a]
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
