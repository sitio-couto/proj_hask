import Data.List.Split

main = do
  text <- readFile "in0.txt"
  let contents = splitData $ lines text
      bus = getBusData $ contents!!1
      graph = buildGraph $ contents!!0
      (start:[finish]) = splitOn " " $ head (contents!!2)
      in
        print graph

getBusData [] = []
getBusData (x:xs) = (a,read b::Float):getBusData xs
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
