module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Model.Classic
import Symbolic.Expr
import Symbolic.STVar
import Symbolic.Term
import Symbolic.Var

kernelOutputTest :: TestTree
kernelOutputTest = testGroup "Output" [testCase "1 1" (runModel 1 1 @?= out11)]
  where out11 = KernelOutput{numberOfEqs=1,numberOfLevels=1,levelSize=[1]}

main = defaultMain kernelOutputTest
