import Data.List

main = do
  file <- getContents
  let paths:transport:[points] = splitData $ map (words) (lines file)
      waitTimes = getTransportData transport
      graph = reduce (mergePaths waitTimes $ buildGraph waitTimes paths)
      in
        mapM_ (print) $ getOutput points graph

-- ORGANIZING INPUT ------------------------------------------------------------

getTransportData ip = foldr (\(a:[b]) c -> (a,(read b::Float)/2):c) [] ip

splitData ip = foldr (\x c-> if x==[] then []:c else (x:head c):tail c) [[]] ip

-- BUILDING INITIAL GRAPH FROM INPUT -------------------------------------------

buildGraph b ip = foldr (\y c-> addE y c) (addV ip) (craftE ip b)

addV x = map (\x->(head x,[])) $ group (sort $ foldr (\(n:v:_) c-> n:v:c) [] x)

addE e g = map (\(v,es)-> if v == fst e then (v,(snd e):es) else (v,es)) g

craftE x b = map (\(n:v:t:[w])->(n,(v,(test b t $ read w::Float),t))) x
  where test b t w = if bf/=[] then w+(snd $ head bf) else w
          where bf = filter (\y->t == fst y) b

-- REARRANGING BUS PATHS ------------------------------------------------------

-- mergePaths Checked
mergePaths b g = foldr (\x c-> foldr (\y k-> addPaths y k x) c c) g b

-- addPaths Checked
addPaths (v,e) g (l,wt) = foldr (\x c -> addE (v,x) c) g $ combE paths wt []
  where paths = tracePaths g e l [] [v]

-- tracePaths checked
tracePaths g pe b p visited
  | test = tracePaths g (getE v g) b (p++next) (v:visited)
  | otherwise = p
  where test = (next /= [])&&(not $ elem v visited)
        (v,_,_) = head next
        next = filter (\(_,_,mode) -> mode == b) pe

-- GetE checked
getE target g = snd (head $ filter (\(v,e) -> v == target) g)

-- combE Checked
combE [] _ c = c
combE (_:[]) _ c = c
combE ((ov,ow,ot):(nv,nw,nt):ps) wt c = combE (x:ps) wt (x:c)
  where x = (nv,(fromIntegral $ floor ((ow+nw-wt)*10))/10,ot++" "++ov++" "++nt)

-- REDUCING MULTI GRAPH TO SIMPLE GRAPH ----------------------------------------

-- reduce checked
reduce [] = []
reduce ((v,es):gs) = (v,rmDups es):reduce gs
  where rmDups e = map (head) $ groupBy (\(x,_,_) (y,_,_)->x==y) (sort e)

-- EXECUTING DIJKSTRA'S ALGORITHIM ---------------------------------------------

-- Creates list of vertex with:(vertex,predecessor,trasnport,weigth,closure)
sPath o g = foldr (\(v,_) pl-> test pl v) [] g
  where test pl v = if v/=o then (v,"","",1/0,True):pl else (v,"","",0,True):pl

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
      where (se,we,y) = e
            (vp,_,_,wp,open) = p
    minWeigth old new = if wo<wn then old else new
      where (_,_,_,wo,_) = old
            (_,_,_,wn,_) = new

-- BACKTRACK PATH FROM FINISH TO START -----------------------------------------

backtrack "" _ = ""
backtrack f sp = (backtrack pv sp)++" "++t++" "++f
  where [(_,pv,t,_,_)] = filter (\(a,b,c,d,e)-> a==f) sp

getOutput [start:[end]] graph = [a,show b]
  where [(_,_,_,b,_)] = filter (\(v,_,_,_,_)-> v==end) c
        a = (drop 2 $ backtrack end c)
        c = dijkstras (sPath start graph) graph

-- TO FURTHER REMOVAL ----------------------------------------------------------
