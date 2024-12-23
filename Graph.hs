{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
type Sommet = Int
type Arete = (Sommet, Sommet)

data  Graphe = Graphe {sommets :: [Sommet] , aretes :: [Arete]} deriving Show


g1 :: Graphe
g1 = Graphe [1,2,3,4,5] [(1,2),(2,3),(2,5),(3,1),(4,1)]

is_elem :: Sommet -> Graphe -> Bool 
is_elem s g = elem s $ sommets g 

neighbors :: Sommet -> Graphe -> [Sommet]
neighbors s g = map (\(a,b) -> a+b-s) $ filter (\p -> s `elem` [fst p, snd p]) $ aretes g

degree :: Sommet -> Graphe ->  Int 
degree s g =length $ neighbors s g

