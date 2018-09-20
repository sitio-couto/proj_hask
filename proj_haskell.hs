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
getE target g = snd (head $ filter (\(v,_) -> v == target) g)

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

-- Creates list of vertex with:(weigth,closure,vertex,predecessor,trasnport)
sPath o g = sort $ foldr (\(v,_) pl-> mq pl v) [] g
  where mq pl v = if v/=o then (False,1/0,v,"",""):pl else (False,0,v,"",""):pl

-- Dijkstras algorithim to find shortest path
dijk ((True,b,c,d,e):sp) _ = (True,b,c,d,e):sp
dijk ((a,b,c,d,e):sp) g = dijk (sort $ (True,b,c,d,e):tryRelax sp (getE c g)) g
  where tryRelax sp es = foldr (\e k-> map (\x-> test e x) k) sp es
        test (v,w,t) x = if (v==j)&&(w+b<i) then (h,w+b,j,c,t) else x
          where (h,i,j,k,l) = x

-- BACKTRACK PATH FROM FINISH TO START -----------------------------------------

backtrack "" _ = ""
backtrack f sp = (backtrack pv sp)++" "++t++" "++f
  where [(_,_,_,pv,t)] = filter (\(a,b,c,d,e)-> c==f) sp

getOutput [start:[end]] graph = [(drop 2 $ backtrack end shortPaths),show b]
  where [(_,b,_,_,_)] = filter (\(_,_,v,_,_)-> v==end) shortPaths
        shortPaths = dijk (sPath start graph) graph

-- TO FURTHER REMOVAL ----------------------------------------------------------
