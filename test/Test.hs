module Main where
import Test.QuickCheck
import Test.Hspec
import Model.Classic
import Symbolic.STVar
import Symbolic.Var
import Symbolic.Term
import Symbolic.Expr
import qualified Symbolic.Amap as A
import qualified Data.Map as M

testNumberOfEquations = hspec $ do
  describe "Number Of Equations"  $ do
    testNumber 1 1 1
    testNumber 1 3 19
    testNumber 2 3 394
    testNumber 3 2 698
  where testNumber n m res =
          it ("N = " ++ show n ++ " and M = " ++ show m ++ " should produce " ++ show res) $ do
            numberOfEqs (runModel n m) `shouldBe` res

amapIsNonZeroCounter :: [(String, Integer)] -> [(String, Integer)] -> Bool
amapIsNonZeroCounter a b = A.getAMap (n <> m) == resMap
  where n = A.fromList a
        m = A.fromList b
        resMap = M.filter (/= 0) $ M.fromListWith (+) (a ++ b)

testAmap = hspec $ do
  describe "Amap properties" $ do
    it "Amap should be a non zero counter" $ do
      property (uncurry amapIsNonZeroCounter)

-- TODO: Test if Amap is actually a monoid



instance (Ord a, Arbitrary a, Arbitrary b, Num b, Eq b) => Arbitrary (Amap a b) where
  arbitrary = A.fromList <$> arbitrary


instance Arbitrary VarType where
  arbitrary = oneof [return RV, return Cnt]

instance Arbitrary Var where
  arbitrary = Var <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Term where
  arbitrary = Term <$> arbitrary

instance Arbitrary Expr where
  arbitrary = Expr <$> arbitrary

testExpr = hspec $ do
  describe "Expr properties" $ do
    it "(x + y) + z == x + (y + z)" $ do
      property (\(x,y,z) -> ((x :: Expr) + y) + z == x + (y + z))
    it "(x * y) * z == x * (y * z)" $ do
      property . withMaxSuccess 30 $ (\(x,y,z) -> ((x :: Expr) * y) * z == x * (y * z))

    it "x + y == y + x" $ do
      property (\(x,y) -> x + y == y + (x :: Expr))
    it "x * y == y * x" $ do
      property (\(x,y) -> x * y == y * (x :: Expr))

    it "x + 0 == x" $ do
      property (\x -> x + (0 :: Expr) == x)
    it "x*1 == x" $ do
      property (\x -> x * (1 :: Expr) == x)

    it "x - x == 0" $ do
      property (\x -> x - x == (0 :: Expr))

    it "x * (y + z) == x*y + x*z" $ do
      property (\(x,y,z) -> (x :: Expr) * (y + z) == x*y + x*z)



main = sequence [testAmap, testNumberOfEquations]
