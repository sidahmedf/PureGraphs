type Vertex = Int
type Edge = (Vertex, Vertex)

data  Graph = Graph {vertices :: [Vertex] , edges :: [Edge]} deriving Show


g1 :: Graph
g1 = Graph [1,2,3,4,5] [(1,2),(2,3),(2,5),(3,1),(4,1)]

isElem :: Vertex -> Graph -> Bool 
isElem s g = elem s $ vertices g 

neighbors :: Vertex -> Graph -> [Vertex]
neighbors s g = map (\(a,b) -> a+b-s) $ filter (\p -> s `elem` [fst p, snd p]) $ edges g

degree :: Vertex -> Graph ->  Int 
degree s g =length $ neighbors s g


--TODO 
path :: [Int] -> Graph 
path l = Graph l (zip l $ drop  1 l)

cycle :: [Int] -> Graph
cycle [] = Graph [] []
cycle l =  Graph l  $ (head l, last l) : zip l (drop 1 l)


kGraph :: Int -> Graph
kGraph = 

addVertex  = undefined 
