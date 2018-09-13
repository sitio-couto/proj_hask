import Data.List.Split
data Edge  a = Edge (a, String, Float) deriving (Show, Read, Eq)
data Node  a = Node (a,[Edge a]) deriving (Show, Read, Eq)
data Bus   a = Bus (String, Float)

g0 = [("a",[("b","a-pe",0.4),("d","linha-370",0.1)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)]),("c",[("b","a-pe",0.5),("d","a-pe",0.3)])]
g1 = [("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("f",[("h","a-pe",12.3),("h","linha-567",1.2)]),("h",[])]
g = g0++g1

addEdge node link ((v,es):graph)
  | (node == v) = (v,(link:es)):graph
  | otherwise = (v,es):addEdge node link graph

-- mergeBusPaths :: [Node a] -> [Bus b] -> [Node a]
mergeBusPaths g [] = g
mergeBusPaths g (b:bs) = foldVertex (mergeBusPaths g bs) g b

-- foldVertex :: [Node a] -> [Node a] -> Bus b -> [Node a]
foldVertex [] g _ = g
foldVertex (gi:gs) g b = addPaths gi (foldVertex gs g b) b

-- addPaths :: Node a -> [Node a] -> Bus b -> [Node a]
addPaths (v,e) g b
  | (length p < 2) = g
  | otherwise = foldr (\x acc -> addEdge o x acc) g (combEdges p $ last b)
  where (o,p) = ("a",[])--tracePaths g e b (v,[]) [v]

-- seekPaths :: [Node a] -> [Edge a] -> Bus b -> Node a -> [a] -> (a,[Edge a])
-- tracePaths g pe b (o,p) visited
--   | test = tracePaths g (getE g v) b (o, p++next) (v:visited)
--   | otherwise = (o,p)
--   where
--     test = (next /= [])&&(not $ elem v visited)
--     (v,_,_) = next
--     next = filter (\(_,mode,_) -> mode == b) pe

combEdges ((v,t,w):p) wt = combEdges' (tail p) [(joinEdges e0 $ head p)]
  where e0 = (v,t,w+wt)

combEdges' [] acc = acc
combEdges' p acc = combEdges' (tail p) $ (joinEdges (head acc) $ head p):acc

-- joinEdges Checked
joinEdges (ov,ot,ow) (v,t,w) = (v,ot++" "++ov++" "++t,ow+w)

-- GetE checked
getE target g = edges
  where (_,edges) = head $ filter (\(v,e) -> v == target) g
