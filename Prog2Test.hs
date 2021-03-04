import Prog2
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit tests"
  [
      -- isSingleDigit :: Integer -> Bool
      testCase "test_threeDifferent1" $ assertEqual [] True (threeDifferent 3 2 1),
      testCase "test_threeDifferent2" $ assertEqual [] False (threeDifferent 3 3 1),
      testCase "test_threeDifferent3" $ assertEqual [] False (threeDifferent 3 3 3),

      -- dividesEvenly :: Integer -> Integer -> Bool
      testCase "test_sum'1" $ assertEqual [] 15 (sum' 5),
      testCase "test_sum'2" $ assertEqual [] (3) (sum' (2)),  --XXX
      testCase "test_sum'3" $ assertEqual [] 5050 (sum' 100),
      
      testCase "test_abssum1" $ assertEqual [] 15 (abssum 0 5),
      testCase "test_abssum2" $ assertEqual [] 45 (abssum (-10) (-5)),      -- XXX
      testCase "test_abssum3" $ assertEqual [] 0 (abssum 0 (-100)),
      
      testCase "test_integerSqrt1" $ assertEqual [] 7 (integerSqrt 50),
      testCase "test_integerSqrt2" $ assertEqual [] 10 (integerSqrt 100),
  
      testCase "test_exponent'1" $ assertEqual [] 256 (exponent' 2 8),
      testCase "test_exponent'2" $ assertEqual [] 100 (exponent' (-10) 2),
    
      testCase "test_largeSmall1" $ assertEqual [] (100, 5) (largeSmall (5, 10, 100)),
      testCase "test_largeSmall2" $ assertEqual [] (100, 100) (largeSmall (100, 100, 100)), 
      testCase "test_largeSmall3" $ assertEqual [] (100, 1) (largeSmall (100, 100, 1)),
      
      testCase "test_swap1" $ assertEqual [] ('a', 'c', 'b', 'd') (swap ('a','b','c','d')),
      testCase "test_swap2" $ assertEqual [] ('f', 'c', 'f', 'f') (swap ('f','f','c','f')),
     
      testCase "test_negateOdds1" $ assertEqual [] [-1, 2, -3, 4, -5] (negateOdds [1,2,3,4,5]),
      testCase "test_negateOdds2" $ assertEqual [] [2,4,6,8] (negateOdds [2,4,6,8]),
      testCase "test_negateOdds3" $ assertEqual [] [1,-2,3,4,-5] (negateOdds [-1,-2,-3,4,5]),
   
      testCase "test_matches1" $ assertEqual [] [3,3,3,3,3] (matches 3 [3,4,3,4,3,4,3,3]),
      testCase "test_matches2" $ assertEqual [] [-20, -20] (matches (-20) [-20, -15, -20]),
      
     testCase "test_element1" $ assertEqual [] True (element (-20) [-20, 10, 2, -2, 0]),
     testCase "test_element2" $ assertEqual [] False (element (10) [0, 1, 2, 3]),
     testCase "test_element3" $ assertEqual [] True (element (0) [0]),
     testCase "test_element4" $ assertEqual [] True (element (1) [1, 1, 1, 1, 1]) 
   ]
