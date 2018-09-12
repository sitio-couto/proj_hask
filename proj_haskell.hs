import Data.List.Split
g = ["a b a-pe 0.4","b a a-pe 0.6","b c a-pe 0.5","c d a-pe 0.3","f h linha-567 1.2"]

buildGraph [] = []
buildGraph (x:xs) = addEdge n (v,t,read w::Float) (buildGraph xs)
  where (n:v:t:[w]) = splitOn " " x

addEdge node link [] = [(node,[link])]
addEdge node link ((v,es):graph)
  | (node == v) = (v,(link:es)):graph
  | otherwise = (v,es):addEdge node link graph
