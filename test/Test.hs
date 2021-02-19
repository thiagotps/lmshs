module Main where
import Test.QuickCheck
import Test.Hspec
import Model.Classic
import Symbolic.STVar

testNumberOfEquations = hspec $ do
  describe "Number Of Equations"  $ do
    testNumber 1 1 1
    testNumber 1 3 19
    testNumber 2 3 394
    testNumber 3 2 698
  where testNumber n m res =
          it ("N = " ++ show n ++ " and M = " ++ show m ++ " should produce " ++ show res) $ do
            numberOfEqs (runModel n m) `shouldBe` res

main = testNumberOfEquations
