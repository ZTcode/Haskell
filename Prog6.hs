{- ##################################
   Tyler Zuraski 
   Homework 6. 
   ################################# -}

module Prog6 where

data Tree1 = Leaf1 Int
        | Node1 Int Tree1 Tree1

preorder :: Tree1 -> [Int]
preorder (Leaf1 v) = [v]
preorder (Node1 i t1 t2) = i: preorder t1 ++ preorder t2

postorder :: Tree1 -> [Int]
postorder (Leaf1 v) = [v]
postorder (Node1 i t1 t2) =   postorder t1 ++ postorder t2 ++ [i] 


sumPositives :: Tree1 -> Int
sumPositives x = sum [ y| y<- preorder x, y >= 0]

countLeaves :: Tree1 -> Int
countLeaves (Leaf1 v) = 1
countLeaves (Node1 i t1 t2) = countLeaves t1 + countLeaves t2

depth :: Tree1 -> Int 
depth (Leaf1 v) = 0
depth (Node1 i t1 t2) = 1 + max (depth t1) (depth t2) 
 
data Tree2 a = Leaf2 a 
             | Node2 [Tree2 a]


occurs :: Eq a => a -> Tree2 a -> Bool 
occurs x (Leaf2 y) = x==y 
occurs x (Node2 (t:[])) = occurs x t 
occurs x (Node2 (t:u:[])) = occurs x t || occurs x u 
occurs x (Node2 (t:u:v))  = occurs x t || occurs x (Node2 (u:v))



countInteriorNodes :: Tree2 a -> Int
countInteriorNodes (Leaf2 v) = 0 
countInteriorNodes (Node2 (ts)) = 1 +  sum(map countInteriorNodes ts)  



sumTree :: Tree2 Int -> Int 
sumTree (Leaf2 v) = v 
sumTree (Node2 (ts)) = sum(map sumTree ts)



pre2 :: Tree2 a -> [a]
pre2 (Leaf2 v) = [v]
pre2 (Node2 (ts)) = concat(map pre2 ts)



depthK :: Int -> Tree2 a -> [a] 
depthK 0 (Leaf2 v) = [v] 
depthK k (Leaf2 v) = [] 
depthK k (Node2 (ts)) = concat(map(depthK (k-1)) ts)   
