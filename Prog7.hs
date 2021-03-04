{- ######################################
   Tyler Zuraski 
   Homework 7. 
   ###################################### -} 

module Prog7 where

unique' :: Eq a => [a] -> [a] 
unique' [] = [] 
unique' (x:xs)
  |not (elem x xs) = x :  unique' xs 
  | otherwise      =  unique' xs 


unique :: Eq a => [a] -> [a]
unique _ = [] 
{--

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = elem x (map(unique xs))

--}


-----------------------------------------------
data Expr1 = Val1 Int 
           | Add1 Expr1 Expr1 
           | Sub1 Expr1 Expr1 

data Expr2 = Val2 Int
           | Add2 Expr2 Expr2 
           | Sub2 Expr2 Expr2 
           | Mul2 Expr2 Expr2 
           | Div2 Expr2 Expr2 

data Expr3 = Val3 Int 
           | Add3 Expr3 Expr3 
           | Sub3 Expr3 Expr3 
           | Mul3 Expr3 Expr3 
           | Div3 Expr3 Expr3 
           | If BExpr3 Expr3 Expr3 

data BExpr3 = BoolLit Bool 
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3 
            | LessThan Expr3 Expr3 
-------------------------------------------------

bEval :: BExpr3 -> Bool
bEval _ = True 

{--
value3 :: Expr3 -> Maybe Int
value3 (Val3 v) = Just v 
value3 (Add2 x y) = addE (value3 x) (value3 y)
value3 (Sub2 x y) = subE (value3 x) (value3 y)
value3 (Mul2 x y) = mulE (value3 x) (value3 y)
value3 (Div2 x y) = divE (value3 x) (value3 y)
--}


value3 :: Expr3 -> Maybe Int
value3 _ = Nothing 


value1 :: Expr1 -> Int
value1 (Val1 v) =  v
value1 (Add1 x1 x2) = (value1 x1) + (value1 x2)
value1 (Sub1 x1 x2) = (value1 x1) - (value1 x2) 

addE :: Maybe Int -> Maybe Int -> Maybe Int
addE Nothing (Just x) = Nothing 
addE (Just x) Nothing = Nothing 
addE (Just x) (Just y) = Just(x + y)

subE :: Maybe Int -> Maybe Int -> Maybe Int
subE Nothing (Just x)  = Nothing 
subE (Just x) Nothing  = Nothing 
subE (Just x) (Just y) = Just(x - y)

mulE :: Maybe Int -> Maybe Int -> Maybe Int
mulE Nothing (Just x) = Nothing 
mulE (Just x) Nothing = Nothing
mulE (Just x) (Just y) = Just(x * y) 

divE :: Maybe Int -> Maybe Int -> Maybe Int
divE _ (Just 0) = Nothing 
divE (Just x) (Just y) = Just(div x y)

value2 :: Expr2 -> Maybe Int
value2 (Val2 v) = Just v
value2 (Add2 x y) = addE (value2 x) (value2 y)
value2 (Sub2 x y) = subE (value2 x) (value2 y)  
value2 (Mul2 x y) = mulE (value2 x) (value2 y)
value2 (Div2 x y) = divE (value2 x) (value2 y)

instance Show Expr2 where
  show (Val2 v)   = show v 
  show (Add2 x y) = "(" ++ show x ++ "+" ++ show y ++ ")"
  show (Sub2 x y) = "(" ++ show x ++ "-" ++ show y ++ ")"
  show (Mul2 x y) = "(" ++ show x ++ "*" ++ show y ++ ")"
  show (Div2 x y) = "(" ++ show x ++ "/" ++ show y ++ ")" 


isVowel :: Char -> Bool
isVowel v 
  | elem v "aeior"  = True 
  | otherwise = False
--starts with vowel

piggy :: String -> String
piggy (x:xs) = (x:xs) ++ "yay"

---starts with non-vowel

piggs :: String -> String
piggs (x:xs)
  | (isVowel x) = x:xs 
  | otherwise = piggs (xs++[x])


piglatinize :: String -> String 
piglatinize (x:xs) 
  | (isVowel x) = piggy (x:xs)
  | otherwise = piggs (x:xs) ++ "ay"



{--
piglatinize :: String -> String 
piglatinize p 
  | elem (head p) "aeiou" = p ++ "ay"
  | otherwise = piglatinize ((tail p) ++ [head p])


piglatin :: String -> String 
piglatin (x:xs)
  | (isVowel x) = x:xs ++ "yay" 
  | otherwise = xs ++ [x] ++ "ay" 
--}


data Tree a = Leaf a | Node (Tree a) (Tree a) 



size :: Tree a -> Int
size (Leaf n)    = 1
size (Node x z) = size x + size z + 1
 
balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = 
    let diff = abs (size l - size r) in
    diff <= 1 && balanced l && balanced r


