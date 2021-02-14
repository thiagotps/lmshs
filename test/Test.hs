module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Model.Classic
import Symbolic.Expr
import Symbolic.STVar
import Symbolic.Term
import Symbolic.Var

gamma n = defaultVar{name='γ', index1=Just n}

indReduceTuples = [(term1, res1), (term2, res2), (term3, res3)]
  where term1 = foldMap toTerm [(a 0, 2 :: Int), (u 0, 2), (vik 1 0, 2)]
        res1 = (STVar $ toTerm (vik 1 0, 2 :: Int), toExpr (a 0) ^ 2 * toExpr (gamma 2))
        term2 = foldMap toTerm [(a 0, 1 :: Int), (a 1, 1), (u (-1), 1), (u 0, 1), (vik 1 0, 2)]
        res2 = (STVar $ foldMap toTerm [(vik 1 0, 2 :: Int), (u (-1), 1)], 0)
        term3 = foldMap toTerm [(a 1, 2 :: Int), (u (-1), 2), (vik 1 0, 2)]
        res3 = (STVar $ foldMap toTerm [(u (-1), 2 :: Int), (vik 1 0, 2)], toExpr (a 1) ^ 2)


indReduceTest :: TestTree
indReduceTest = testGroup "indReduce" [testCase (show term) (indReduce isInd reduce term @?= res) | (term, res) <- indReduceTuples]


kernelOutputTest :: TestTree
kernelOutputTest = testGroup "Output" [testCase "1 1" (runModel 1 1 @?= out11)]
  where out11 = KernelOutput{numberOfEqs=1,numberOfLevels=1,levelSize=[1]}

allTests :: TestTree
allTests = testGroup "Test" [indReduceTest, kernelOutputTest]

main = defaultMain allTests
