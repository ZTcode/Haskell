{- ##################################
  Tyler Zuraski
  Homework 8.
 ################################## -}

module Prog8 where


sumSqNeg :: [Int] -> Int
sumSqNeg (xs) = sum(map (^2) (filter (<0) (xs)))


containing :: Eq a => [a] -> [a] -> Bool
containing [] _ = True 
containing (x:xs) (ys)
  | x `elem` ys = containing (xs) (ys) 
  | otherwise = False 


total :: (Int -> Int) -> [Int] -> Int
total f (xs) = sum(map f xs)



---
containing' :: Eq a => [a] -> [a] -> Bool
containing' (xs) ys = and(zipWith (==) (ys) (xs))


lengths :: [String] -> [Int]
lengths (xs) = map length xs


product' :: Num a => [a] -> a
product' xs = product(map (+0) (xs))


f :: Ord a => a -> a
f x = x

max' :: Ord a => [a] -> a
max' xs = maximum(map (f) (xs))

append' :: [a] -> [a] -> [a]
append' xs ys = foldr (\x y -> x:y) ys xs




removeF :: [Bool] -> [a] -> [a]
removeF _ [] = [] 
removeF (True:ds) (x:xs) = x : removeF ds xs 
removeF (False:ds) (x:xs) = xs 



filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p xs = removeF (map (p) xs) xs



filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = reverse (filterFirst p (reverse xs))



