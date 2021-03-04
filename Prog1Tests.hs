{- ##################################
Richard Burns
Units Tests for Homework 1.

Usage: ghci Prog1Tests; main

Dependencies: cabal install tasty
              cabal install tasty-hunit

   ################################## -}


import Prog1
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment


main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

{-
main = defaultMain tests
-}

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit tests"
  [

      -- isSingleDigit :: Integer -> Bool
      testCase "test_isSingleDigit1" $ assertEqual [] True (isSingleDigit 3),
      testCase "test_isSingleDigit2" $ assertEqual [] False (isSingleDigit (-13)),
      testCase "test_isSingleDigit3" $ assertEqual [] True (isSingleDigit 0),

      -- dividesEvenly :: Integer -> Integer -> Bool
      testCase "test_dividesEvenly1" $ assertEqual [] False (dividesEvenly 14 3),
      testCase "test_dividesEvenly2" $ assertEqual [] True (dividesEvenly 12 3),
      testCase "test_dividesEvenly3" $ assertEqual [] False (dividesEvenly (-4) 3),
      testCase "test_dividesEvenly4" $ assertEqual [] True (dividesEvenly (-4) 2),

      -- middle :: Integer -> Integer -> Integer -> Integer
      testCase "test_middle1" $ assertEqual [] 3 (middle 2 14 3),
      testCase "test_middle2" $ assertEqual [] 3 (middle 12 3 1),
      testCase "test_middle3" $ assertEqual [] (-1) (middle (-4) 3 (-1)),
      testCase "test_middle4" $ assertEqual [] (-4) (middle (-4) (-4) 2),
      testCase "test_middle5" $ assertEqual [] 9 (middle 3 9 10),

      -- nand :: Bool -> Bool -> Bool
      testCase "test_nand1" $ assertEqual [] False (nand True True),
      testCase "test_nand2" $ assertEqual [] True (nand True False),
      testCase "test_nand3" $ assertEqual [] True (nand False True),
      testCase "test_nand4" $ assertEqual [] True (nand False False),

      -- triangleArea :: Integer -> Integer -> Float
      testCase "test_triangleArea1" $ assertEqual [] 2 (triangleArea 2 2),
      testCase "test_triangleArea2" $ assertEqual [] 6 (triangleArea 4 3),
      testCase "test_triangleArea3" $ assertEqual [] 20 (triangleArea 4 10),

      -- floorDecimal :: Float -> Float
      testCase "test_floorDecimal1" $ assertEqual [] 13.0 (floorDecimal 13.5),
      testCase "test_floorDecimal2" $ assertEqual [] (-14.0) (floorDecimal (-13.3)),
      testCase "test_floorDecimal3" $ assertEqual [] 12.0 (floorDecimal 12.2),

      -- isNotALetter :: Char -> Bool
      testCase "test_isNotALetter1" $ assertEqual [] False (isNotALetter 'a'),
      testCase "test_isNotALetter2" $ assertEqual [] True (isNotALetter '9'),
      testCase "test_isNotALetter3" $ assertEqual [] False (isNotALetter 'E'),
      testCase "test_isNotALetter4" $ assertEqual [] True (isNotALetter ' '),

      -- letterGrade :: Integer -> String
      testCase "test_letterGrade1" $ assertEqual [] "A" (letterGrade 93),
      testCase "test_letterGrade2" $ assertEqual [] "B" (letterGrade 84),
      testCase "test_letterGrade3" $ assertEqual [] "C+" (letterGrade 79),
      testCase "test_letterGrade4" $ assertEqual [] "F" (letterGrade 19),

      -- averageThree :: Integer -> Integer -> Integer -> Float
      testCase "test_averageThree1" $ assertEqual [] 98 (averageThree 100 97 97),
      testCase "test_averageThree2" $ assertEqual [] 1 (averageThree 3 (-2) 2),
      testCase "test_averageThree3" $ assertEqual [] 25 (averageThree 25 25 25),

      -- howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
      testCase "test_howManyBelowAverage1" $ assertEqual [] 2 (howManyBelowAverage 100 97 97),
      testCase "test_howManyBelowAverage2" $ assertEqual [] 1 (howManyBelowAverage 3 (-2) 2),
      testCase "test_howManyBelowAverage3" $ assertEqual [] 0 (howManyBelowAverage 25 25 25),
      testCase "test_howManyBelowAverage4" $ assertEqual [] 1 (howManyBelowAverage 1000000 1000000 999999)

  ]
