{- ####################################
     Tyler Zuraski 
     Homework 1
    ################################## -}

module Prog2 where 

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c
  | a == b || a == c = False
  | b == c           = False
  | otherwise        = True


sum' :: Integer -> Integer
sum' 1 = 1
sum' n = (n) + sum' (n-1)




abssum :: Integer -> Integer -> Integer 
abssum m n
  | m >  n = 0 
  | otherwise = abs m + (abssum (m + 1) n)



integerSqrt :: Integer -> Integer
integerSqrt n = floor(sqrt (fromIntegral n))


exponent' :: Integer -> Integer -> Integer
exponent' b 1 = b * 1 
exponent' b e  = b * (exponent' b (e - 1))


---helper function
threeMax :: Integer -> Integer -> Integer -> Integer 
threeMax x y z 
  | x >= y && x >=z = x
  | y >= z          = y 
  | otherwise       = z 
---helper function 
threeMin :: Integer -> Integer -> Integer -> Integer
threeMin x y z 
  | x <= y && x <= z = x
  | y <= z           = y
  | otherwise        = z


-- returns (Large, Small)
largeSmall :: (Integer, Integer, Integer) -> (Integer, Integer)
largeSmall (a,b,c) = ((threeMax a b c), threeMin a b c) 

swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (a, b, c, d) = (a, c, b, d)



matches :: Integer -> [Integer] -> [Integer]
matches n l = [x | x <- l, x==n] 




element :: Integer -> [Integer] -> Bool
element n l 
  | matches n l == [] = False 
  | otherwise = True 



{--


divisors :: Int -> [Int] 
divisors n = [ x | x <- [1..n], n `mod` x == 0]


--} 



makeOdd :: Integer -> Integer
makeOdd x
  | x `mod` 2 == 0 = x
  | otherwise = x * (-1)




negateOdds :: [Integer] -> [Integer]
negateOdds a = [makeOdd x | x <- a]  
