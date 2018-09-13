import Data.List.Split
data Edge  a = Edge (a, String, Float) deriving (Show, Read, Eq)
data Graph a = Graph [(a, [Edge])] deriving (Show, Read, Eq)
data Bus   a = Bus (String, Float)

main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      bus = Bus (getBusData $ contents!!1)
      graph = Graph (buildGraph $ contents!!0)
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

-- topVertex _ [] acc = acc
-- topVertex o ((v,e):gs) acc
--   | (o == v) = [(v,e),acc:gs]
--   | otherwise = topVertex o gs ((v,e):acc)

mergeBusPath :: Graph -> [Bus] -> Graph
mergeBusPath g [] = g
mergeBusPath g (b:bs) = foldVertex (mergeBusPath g bs) g b

foldVertex :: Graph -> Graph -> [Bus] -> Graph
foldVertex [] g _ = g
foldVertex (o:gs) g b = tracePath o (foldVertex gs g b) b

tracePath :: Graph -> Graph -> [Bus] -> Graph
tracePath ((v,e):gs) g b
  | (paths == (_,[]) = g
  | otherwise = combEdges paths wt --TODO checkar o que fazer aki
  where paths = seekPaths g e b (v,[]) [v]
        (_,wt) = b

seekPaths :: Graph -> [Edge] -> [Bus]
seekPaths g pe b (o,p) visited
  | test = seekPaths g (getE g $ head next) b (o,n:p) (head next):visited
  | otherwise = (o,p)
  where
    test = (next /= [])&&(not $ elem (head next) visited)
    next = filter (/(_,mode,_) -> mode == b!!0) pe

combEdges :: (a,[Edge]) -> Float -> [Edge]
combEdges (_,[]) _ = g
combEdges (_,[_]) _ = g
combEdges (o,(v,t,w):p) wt = combEdges' (tail p) [(joinPaths b $ head p)]
                             where b = (v,t,w+(wt/2))

combEdges' :: [Edge] -> [Edge]
combEdges' [] acc = acc
combEdges' p acc = combEdges' (tail p) $ (joinEdges (head acc) $ head p)):acc

joinEdges :: Edge -> Edge -> Edge
joinEdges (ov,ot,ow) (v,t,w) = (v,ot++" "++ov++" "++t,ow+w)

getE :: Graph -> [Edge]
getE g target = edges
  where (_,edges) = head $ filter (/(v,e) -> v == target) g
