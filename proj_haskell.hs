import Data.List.Split
g = ["a b a-pe 0.4","b a a-pe 0.6","b c a-pe 0.5","c d a-pe 0.3","f h linha-567 1.2","f h a-pe 1.0"]
g1 = ["f h linha-567 1.2","f h a-pe 1.0"]
a = ("h","a-pe", read "0.3"::Float)

buildGraph [] = []
buildGraph (x:xs) = addEdge n (v,t,read w::Float) (buildGraph xs)
  where (n:v:t:[w]) = splitOn " " x

addEdge node link [] = [(node,[link])]
addEdge node link ((v,es):graph)
  | (node == v) = (v,shortest link es):graph
  | otherwise = (v,es):addEdge node link graph

shortest link [] = [link]
shortest (v,t,w) ((x,y,z):xs)
  | (v == x)&&(w < z) = (v,t,w):xs
  | (v == x)&&(w >= z) = (x,y,z):xs
  | otherwise = (x,y,z):shortest (v,t,w) xs
