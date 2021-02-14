module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Model.Classic
import Symbolic.Expr
import Symbolic.STVar
import Symbolic.Term
import Symbolic.Var

gamma n = defaultVar{name='γ', index1=Just n}
a0 = a 0
a1 = a 1
v0k = vik 0 0
uk = u 0
ukl1 = u (-1)

indReduceTuples = [(term1, res1), (term2, res2), (term3, res3)]
  where term1 = foldMap toTerm [(a0, 2 :: Int), (uk, 2), (v0k, 2)]
        res1 = (STVar $ toTerm (v0k, 2 :: Int), toExpr a0 ^ 2 * toExpr (gamma 2))
        term2 = foldMap toTerm [(a0, 1 :: Int), (a1, 1), (ukl1, 1), (uk, 1), (v0k, 2)]
        res2 = (STVar $ foldMap toTerm [(v0k, 2 :: Int), (ukl1, 1)], 0)
        term3 = foldMap toTerm [(a1, 2 :: Int), (ukl1, 2), (v0k, 2)]
        res3 = (STVar $ foldMap toTerm [(ukl1, 2 :: Int), (v0k, 2)], toExpr a1 ^ 2)


indReduceTest :: TestTree
indReduceTest = testGroup "indReduce" [testCase (show term) (indReduce isInd reduce term @?= res) | (term, res) <- indReduceTuples]

expandTest :: TestTree
expandTest = testGroup "expandTest" [testCase (show stvar) (expandIntoSTSum stvar expandFunc isInd reduce @?= mempty),
                                    testCase (show stvar2) (expandIntoSTSum stvar2 expandFunc isInd reduce @?= mempty),
                                    testCase (show stvar3) (expandIntoSTSum stvar3 expandFunc isInd reduce @?= mempty) ]
  where stvar = STVar $ toTerm (v0k, 2 :: Int)
        stvar2 = STVar $ foldMap toTerm [(v0k, 2 :: Int), (ukl1, 2)]
        stvar3 = STVar $ foldMap toTerm [(v0k, 2 :: Int), (ukl1, 4)]
        expandFunc = expandFuncBuilder 1 2

kernelOutputTest :: TestTree
kernelOutputTest = testGroup "Output" [testCase "1 1" (runModel 1 1 @?= out11)]
  where out11 = KernelOutput{numberOfEqs=1,numberOfLevels=1,levelSize=[1]}

allTests :: TestTree
allTests = testGroup "Test" [indReduceTest, kernelOutputTest, expandTest]

main = defaultMain allTests
