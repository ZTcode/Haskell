{- ##################################
     	TYLER ZURASKI
        HOMEWORK 3
   ################################## -}

module Prog3 where

import Data.Char



productLastPart :: Int -> [Int] -> Int
productLastPart n l
  | n == 0     = 0 
  | otherwise  = product(drop((length l) - n) l)


init' :: [Int] -> [Int]
init' a = (take((length a) -1) a)


init'' :: [Int] -> [Int]
init'' [y] = [] 
init'' (x:xs) = x : init'' xs

 
elemAt :: Int -> [Int] -> Int
elemAt ith (x:xs)
  | ith == 1  = x
  | otherwise = elemAt (ith - 1) xs


numTimes :: Int -> [Int] -> Int
numTimes _ [] = 0
numTimes e (x:xs)
  | x == e    = 1 + numTimes e xs  
  | otherwise = numTimes e xs


lowerFirstLetter :: String -> String
lowerFirstLetter a = toLower (head a) : tail a

comparePar :: String -> Char -> Bool
comparePar [] _  = True 
comparePar (x:xs) p 
  | x == p    = comparePar xs p
  | otherwise = False


nestedParens :: String -> Bool
nestedParens a
  | ((length a) `mod` 2) /= 0 = False
  | otherwise = comparePar firstH '(' && comparePar secondH ')' 
  where
    firstH  = take ((length a) `div` 2) a 
    secondH = drop ((length a) `div` 2) a 


--ins from notes 
ins :: (Float, Int, String) -> [(Float, Int, String)] -> [(Float, Int, String)]
ins (a,b,c) [] = [(a,b,c)]
ins (a,b,c) ((x,y,z):xs)
  | b < y     = (a,b,c) : (x,y,z) : xs 
  | otherwise = (x,y,z) : (ins (a,b,c) xs)


iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' [] = []
iSort' [(x,y,z)] = [(x,y,z)]
iSort' ((x,y,z) : xs) = ins (x,y,z) (iSort' xs) 





triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z) | x<-[1..n],y<-[1..n],z<-[1..n],((x^2) + (y^2)) == (z^2)]



merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs 
merge [] ys = ys
merge (x:xs) (y:ys)
  | x >= y = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)


