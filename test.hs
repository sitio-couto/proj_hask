
path = [("b","a-pe",0.4),("d","linha-370",0.1),("f","a-pe",3.0),("c","a-pe",0.3)]
g0 = [("a",[("b","a-pe",0.4),("d","linha-370",0.1)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)]),("c",[("b","a-pe",0.5),("d","a-pe",0.3)])]
g1 = [("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("f",[("h","a-pe",12.3),("h","linha-567",1.2)]),("h",[])]
g00 = [("f",[("h","linha-567",7.2),("h","a-pe",12.3)]),("d",[("c","a-pe",0.3),("f","a-pe",3.0)]),("a",[("b","a-pe",0.4),("d","linha-370",7.6)])]
g11 = [("c",[("b","a-pe",0.5),("d","a-pe",0.3)]),("b",[("a","a-pe",0.6),("c","a-pe",0.5)])]
g3 = [("a",[("b","linha-666",0.5)]),("b",[("c","linha-666",0.2)]),("c",[("d","linha-666",0.1)]),("d",[])]
g =g0++g1

main = do
  file <- getContents
  let contents = splitData $ lines file
      waitTime = getBusData $ contents!!1
      graph = reduce (mergePaths waitTime $ buildGraph waitTime (contents!!0))
      (start:[finish]) = words $ head (contents!!2)
      in
        print (backtrack finish $ dijkstras (sPath start graph) graph)

-- ORGANIZING INPUT ------------------------------------------------------------

getBusData [] = []
getBusData (x:xs) = (a,(read b::Float)/2):getBusData xs
  where (a:[b]) = words x

splitData l = splitData' l []
splitData' :: [String] -> [String] -> [[String]]
splitData' [] acc = [acc]
splitData' (x:xs) acc
  | (x /= "") = splitData' xs $ x:acc
  | otherwise = (reverse acc):splitData' xs []

-- BUILDING INITIAL GRAPH FROM INPUT -------------------------------------------

-- Cria grafo somando tempos de espera de busao
buildGraph _ [] = []
buildGraph b (x:xs) = addVertices v $ addEdge n (v,t,total_w) (buildGraph b xs)
  where total_w = foldl (\c (l,wt) -> if l == t then c+wt else c) (read w::Float) b
        (n:v:t:[w]) = words x

-- Adiciona aresta a um vertice (adiciona o vertice se nao encontra-lo)
addEdge node link [] = [(node,[link])]
addEdge node link ((v,es):g)
  | (node == v) = (v,(link:es)):g
  | otherwise = (v,es):addEdge node link g

-- Garante que vertices sem arestas de saida sejam adicionados
addVertices v g = if (elem v $ map (\(x,_)-> x) g) then g else (v,[]):g

 -- REARRANGING BUS PATHS ------------------------------------------------------

-- mergeBusPaths Checked
mergePaths [] g = g
mergePaths (b:bs) g = foldVertex newGraph newGraph b
  where newGraph = mergePaths bs g

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

-- REDUCING MULTI GRAPH TO SIMPLE GRAPH ----------------------------------------

-- reduce checked
reduce [] = []
reduce ((v,e):gs) = (v,rmDups e []):(reduce gs)

-- rmDups checked
rmDups [] acc = acc
rmDups [e] acc = e:acc
rmDups e acc = rmDups rest (m:acc)
  where rest = foldr (\x c-> filter (\y-> x/=y) c) e k
        m = foldl (\c x-> test x c) (head k) k
        k = filter (\(n,_,_)-> n==v) e
        (v,_,_) = head e
        test (v,t,w) (a,b,c) = if w<c then (v,t,w) else (a,b,c)

-- EXECUTING DIJKSTRA'S ALGORITHIM ---------------------------------------------

-- Creates list of vertex with:(vertex,predecessor,trasnport,weigth,closure)
sPath o g = foldr (\(v,_) pl-> test pl v) [] g
  where test pl v = if v/=o then (v,"","",ub,True):pl else (v,"","",0.0,True):pl
        ub = 999999999999999999999999.1

-- Mark vertex as "closed" when its shortest path is found
closeVertex v sp = foldr (test) [] sp
  where test p ps = if a==v then (a,b,c,d,False):ps else p:ps
          where (a,b,c,d,e) = p

-- Dijkstras algorithim to find shortest path
dijkstras sp g
  | (osp == []) = sp
  | otherwise = dijkstras (closeVertex sv nsp) g
  where
    nsp = foldr (\x c-> foldr (\y ac->(test x y sv sw):ac) [] c) sp sve
    sve = getE sv g
    (sv,_,_,sw,_) = foldl (minWeigth) (head osp) (tail osp)
    osp = filter (\(_,_,_,_,b)->b) sp
    test e p sv sw = if (se==vp)&&(we+sw<wp) then (se,sv,y,we+sw,open) else p
      where (se,y,we) = e
            (vp,_,_,wp,open) = p
    minWeigth old new = if wo<wn then old else new
      where (_,_,_,wo,_) = old
            (_,_,_,wn,_) = new

-- BACKTRACK PATH FROM FINISH TO START -----------------------------------------

backtrack "" _ = ""
backtrack f sp = (backtrack pv sp)++" "++t++" "++f
  where [(_,pv,t,_,_)] = filter (\(a,b,c,d,e)-> a==f) sp
