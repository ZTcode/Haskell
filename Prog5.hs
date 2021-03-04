{- ###############################
   TYLER ZURASKI
   HOMEWORK 5. 
   ############################## -}
module Prog5 where

reverse' :: [a] -> [a]
reverse' l = 
      case l  of 
      [] -> []
      (x:xs) -> (reverse' xs) ++ [x] 

reverse'' :: String -> String
reverse'' str =
       case str of 
       "" -> str
       c:cs -> reverse'' cs ++ [c]

isPalindrome :: String -> Bool
isPalindrome s =
           case s == reverse'' s of 
           True  -> True
           False -> False

safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter s [] = Nothing 
safeFindAfter s (x:xs) 
  | s == x =  Just xs
  | otherwise = safeFindAfter s (xs)

data Set = Set [Char] 
         | EmptySet
    deriving Show 

saferemove :: Char -> Set -> Maybe Set
saferemove _ EmptySet  = Nothing
saferemove c (Set xs)
  | length set' == 0 = Just EmptySet 
  | length set' == length xs = Nothing 
  | otherwise = Just (Set set') 
  where 
    set' = [x | x <- xs , x /= c ] 

removeFromList :: Char -> [Char] -> [Char]
removeFromList _ [] = [] 
removeFromList c (x:xs)
  | c == x = removeFromList c xs 
  | otherwise = x : removeFromList c xs


merge :: Set -> Set -> Set
merge (Set []) (Set ys) = (Set ys)
merge (Set xs) (Set []) = (Set xs)
merge (Set (xs)) (Set (ys)) = Set(xs ++ ys)

add :: Char -> Set -> Set 
add c EmptySet = Set [c] 
add c (Set xs)
  | member c (Set xs) == True =  (Set xs)
  | otherwise = Set (c:xs)





member''' :: Char -> Set -> Bool
member''' c (Set(xs)) = c `elem` xs 

member :: Char -> Set -> Bool
member _ EmptySet = False
member _ (Set[])  = False   
member c (Set(x:xs))
  | c == x = True 
  | c /= x = member c (Set(xs))

        
size :: Set -> Int
size EmptySet = 0 
size (Set []) = 0 
size (Set(_:xs)) = 1 + (size  (Set(xs)))

{--
equal :: Set -> Set -> Bool
equal EmptySet EmptySet = True
equal _ (Set []) = True 
equal (Set (x:xs)) (Set (y:ys)) 
  | member x (Set ys) == True =  equal (Set (xs)) (Set (ys))
  | otherwise = False 

--}
removeRepeats :: [Char] -> [Char]
removeRepeats [] = []
removeRepeats (x:xs)
   | set_len == 0 = x : removeRepeats xs
   | otherwise   = removeRepeats xs
   where
      set_len = length [l | l <- xs, x == l] 

union :: Set -> Set -> Set
union EmptySet (Set ys) = Set ys
union (Set xs) EmptySet = Set xs
union EmptySet EmptySet = EmptySet
union (Set xs) (Set ys) = Set (removeRepeats sorted)
   where 
      sorted = sort (xs ++ ys)


equal :: Set -> Set -> Bool
equal EmptySet EmptySet     = True
equal EmptySet (Set _)      = False
equal (Set _) EmptySet      = False
equal (Set (x:xs)) (Set (y:ys))
   | sort1 == sort2 = True
   | otherwise      = False
   where 
      sort1 = sort (x:xs)
      sort2 = sort (y:ys)


sort :: [Char] -> [Char]
sort [] = []
sort (x:xs) = insertion (x) (sort xs)

insertion :: Char -> [Char] -> [Char]
insertion x [] = [x] 
insertion x (y:ys) 
   | x <= y    = x:y:ys
   | otherwise = y:(insertion x ys)

intersection :: Set -> Set -> Set
intersection EmptySet EmptySet = EmptySet
intersection (Set xs) EmptySet = EmptySet
intersection EmptySet (Set ys) = EmptySet 
intersection (Set xs) (Set ys)
   | length new_set == 0 = EmptySet
   | otherwise           = Set new_set
   where 
      new_set = [m | m <- xs, k <- ys, m == k]



