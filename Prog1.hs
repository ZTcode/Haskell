{- ###################################
	Tyler Zuraski
	Homework 1.
   ################################### -}

module Prog1 where 

--
isSingleDigit :: Integer -> Bool
isSingleDigit a 
  | a > (-10) && a < 10   = True
  | a <= (-10) || a >= 10 = False

--  
dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly a b  
  | (a `div` b) `mod` 2 == 0 = True
  | otherwise        = False

--
middle :: Integer -> Integer -> Integer -> Integer 
middle a b c 
  | a >= b && a <= c = a
  | a >= c && a <= b = a
  | b >= a && b <= c = b 
  | b >= c && b <= a = b
  | otherwise        = c

--
nand :: Bool -> Bool -> Bool
nand a b 
  | a == True && b == True = False
  | otherwise           = True 


--
triangleArea :: Integer -> Integer -> Float
triangleArea a b = (fromIntegral a) * (fromIntegral b) * (1/ 2)

--
floorDecimal :: Float -> Float 
floorDecimal a = (fromIntegral(floor a)) 

--
letterGrade :: Integer -> String
letterGrade x 
  | x >= 93 = "A"
  | x >= 90 = "A-"
  | x >= 87 = "B+"
  | x >= 83 = "B"
  | x >= 80 = "B-" 
  | x >= 77 = "C+"
  | x >= 73 = "C"
  | x >= 70 = "C-"
  | x >= 67 = "D+"
  | x >= 63 = "D"
  | x >= 60 = "D-"
  | x <  60 = "F"


--
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c =  ((fromIntegral a) + (fromIntegral b) + (fromIntegral c))/(fromIntegral 3)

-- 
howManyBelowAverage :: Integer -> Integer -> Integer -> Integer 
howManyBelowAverage a b c 
  | (averageThree a b c) > (fromIntegral a) && (averageThree a b c) > (fromIntegral b) = 2 
  | (averageThree a b c) > (fromIntegral a) && (averageThree a b c) > (fromIntegral c) = 2 
  | (averageThree a b c) > (fromIntegral b) && (averageThree a b c) > (fromIntegral c) = 2
  | (averageThree a b c) > (fromIntegral a) = 1
  | (averageThree a b c) > (fromIntegral b) = 1 
  | (averageThree a b c) > (fromIntegral c) = 1 
  | otherwise 				    = 0  


{--

Returns TrueE if a given character is not a letter 
t
----}
isNotALetter :: Char -> Bool
isNotALetter a 
  | a == 'a' || a == 'b' || a == 'c' || a == 'd' || a == 'e' || a == 'f' || a == 'g' || a == 'h' || a == 'i' || a == 'j' || a == 'k' || a == 'l' || a == 'm' || a == 'n' || a == 'o' || a == 'p' || a == 'q' || a == 'r' || a == 's' || a == 't' || a == 'u' || a == 'v' ||  a == 'w' || a == 'x' || a == 'y' || a == 'z' = False
  | a == 'A' || a == 'B' || a == 'C' || a == 'D' || a == 'E' || a == 'F' || a == 'G' || a == 'H' || a == 'I' || a == 'J' || a == 'K' || a == 'L' || a == 'M' || a == 'N' || a == 'O' || a == 'P' || a == 'Q' || a == 'R' || a == 'S' || a == 'T' || a == 'U' || a == 'V' || a == 'W' || a == 'X' || a == 'Y' || a == 'Z' = False   
  | otherwise  = True 

