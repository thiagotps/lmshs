module Main where
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Model.Classic
import Symbolic.STVar
import Symbolic.Var
import Symbolic.Term
import Symbolic.Expr
import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A
import qualified Data.Map as M
import Data.Bifunctor (first)

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

testAmapHspec = hspec $ do
  describe "Amap properties" $ do
    it "Amap should be a non zero counter" $ do
      property (uncurry amapIsNonZeroCounter)

-- TODO: Test if Amap is actually a monoid

instance (Eq a, Eq b) => EqProp (Amap a b) where
  (=-=) = eq

testAmapMonoid :: IO ()
testAmapMonoid = quickBatch (monoid (A.fromList ([] :: [(String, Integer)]) ))

testAmap = testAmapMonoid >> testAmapHspec


instance (Ord a, Arbitrary a, Arbitrary b, Num b, Eq b) => Arbitrary (Amap a b) where
  arbitrary = A.fromList <$> arbitrary


instance Arbitrary VarType where
  arbitrary = oneof [return RV, return Cnt]

newtype RVar = RVar {getRVar :: Var} deriving (Eq, Show, Ord)
instance Arbitrary RVar where
  arbitrary = let v = Var RV <$> arbitrary <*> arbitrary <*> (Just <$> arbitrary)
              in RVar <$> v


-- TODO: I should derive it using genericArbitrary
instance Arbitrary Var where
  arbitrary = Var <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Term where
  arbitrary = Term <$> arbitrary

instance Arbitrary Expr where
  arbitrary = Expr <$> arbitrary

instance Arbitrary STVar where
  arbitrary = STVar . Term . A.fromList . map (first getRVar) <$> (arbitrary :: Gen [(RVar, Int)])

instance Arbitrary STSum where
  arbitrary = STSum <$> arbitrary

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


testNormalize = hspec $ do
  describe "Normalize properties" $ do
    it "normalize (normalize x) = normalize x" $ do
      property (\x -> normalize (normalize  x) == normalize x)
    it "ignore order" $ do
      property ignoreOrder
  where ignoreOrder m = let l = A.toList . getTerm . getSTVar $ m
                            lr = reverse l
                            n = STVar . Term . A.fromList $ lr
                            in normalize m == normalize n

instance EqProp KernelOutput where
  (=-=) = eq

instance Arbitrary KernelOutput where
  arbitrary = genericArbitrary

  -- arbitrary = KernelOutput <$> arbitrary <*> arbitrary <*> arbitrary

testKernelOutput = quickBatch (monoid (mempty :: KernelOutput))

main = sequence [testAmap, testExpr, testNormalize, testKernelOutput, testNumberOfEquations]
