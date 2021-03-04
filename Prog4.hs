{- #################################
   TYLER ZURASKI
   HOMEWORK 4.
   ################################# -} 

module Prog4 where 

type Month = Int 
type Day   = Int
type Year  = Int
type Date  = (Month, Day, Year)


morerecent :: Date -> Date -> Date
morerecent (a,b,c) (x,y,z)
  | c > z = (a,b,c)
  | c < z = (x,y,z)
  | a > x = (a,b,c)
  | a < x = (x,y,z)
  | b > y = (a,b,c)
  | otherwise = (x,y,z)


numInMonth :: [Date] -> Month -> Int
numInMonth [] _  = 0
numInMonth ((x,y,z):xs) m
  | x == m    = 1 + numInMonth xs m
  | otherwise = numInMonth xs m


datesInMonth :: [Date] -> Month -> [Date]
datesInMonth [] _ = [] 
datesInMonth ((x,y,z):xs) m 
  | x == m    = (x,y,z) : datesInMonth xs m
  | otherwise = datesInMonth xs m


month2Str :: Date -> String
month2Str (m,d,y) 
  | m == 1 = "January" 
  | m == 2 = "February" 
  | m == 3 = "March" 
  | m == 4 = "April" 
  | m == 5 = "May"
  | m == 6 = "June" 
  | m == 7 = "July" 
  | m == 8 = "August" 
  | m == 9 = "September"
  | m == 10 = "October"
  | m == 11 = "November"
  | m == 12 = "December"

date2Str :: Date -> String 
date2Str (m,d,y) = (month2Str (m,d,y)) ++" " ++  (show d)++ ", "  ++ (show y)

monthLookup :: Int -> Int
monthLookup d 
  | d <= 31 = 1
  | d <= 59 = 2 
  | d <= 90 = 3 
  | d <= 120 = 4 
  | d <= 151 = 5 
  | d <= 181 = 6
  | d <= 212 = 7 
  | d <= 243 = 8 
  | d <= 273 = 9 
  | d <= 304 = 10
  | d <= 334 = 11 
  | d <= 365 = 12


monthRange :: Int -> Int -> [Int] 
monthRange m n = [x| x<-[(monthLookup m)..(monthLookup n)]] 

--  | m >  n = [] 
--  | m <= n = monthLookup m : 


validDate :: Date -> Bool
validDate (m,d,y) 
  | y < 0 || y > 9999           = False 
  | m == 1 && d >= 1 && d <= 31 = True
  | m == 2 && d >= 1 && d <= 28 = True 
  | m == 3 && d >= 1 && d <= 31 = True
  | m == 4 && d >= 1 && d <= 30 = True
  | m == 5 && d >= 1 && d <= 31 = True
  | m == 6 && d >= 1 && d <= 30 = True
  | m == 7 && d >= 1 && d <= 31 = True
  | m == 8 && d >= 1 && d <= 31 = True 
  | m == 9 && d >= 1 && d <= 30 = True 
  | m == 10 && d >=1 && d <= 31 = True
  | m == 11 && d >=1 && d <= 30 = True 
  | m == 12 && d >=1 && d <= 31 = True 
  | otherwise = False 

validLeapDate :: Date -> Bool
validLeapDate (m,d,y)
  | m == 2 && d == 29 && y `mod` 4 == 0   = True 
  | m == 2 && d == 29 && y `mod` 400 == 0 = True 
  | otherwise = False


season :: Date -> String
season (m,d,y) 
  | m == 1 || m == 2   = "Winter"
  | m == 4 || m == 5   = "Spring"
  | m == 10 || m == 11 = "Fall" 
  | m == 7 || m == 8   = "Summer"
  | m == 3 && d < 20   = "Winter"
  | m == 3 && d > 19   = "Spring"
  | m == 6 && d < 21   = "Spring"
  | m == 6 && d > 20   = "Summer" 
  | m == 9 && d < 23   = "Summer"
  | m == 9 && d > 22   = "Fall"
  | m == 12 && d < 22  = "Fall"
  | m == 12 && d > 21  = "Winter"


