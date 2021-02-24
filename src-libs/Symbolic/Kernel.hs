-- | 

module Symbolic.Kernel
  ( module Symbolic.Kernel,
    module Symbolic.STVar,
  )
where

import Symbolic.STVar
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Product(Product))
import Data.Semigroup ( Sum(Sum), Sum(getSum) )
import GHC.Generics (Generic)

import qualified Symbolic.Amap as A


type IndFunc = Var -> Var -> Bool
type ExpandFunc = Var -> Expr
type RVReduceFunc = (Var, Int) -> Expr

indReduce :: IndFunc -> RVReduceFunc -> Term -> (STVar, Expr)
indReduce i f (Term m) = let (term, Product expr) = getInd rvList
                         in  (STVar term, cntExpr * expr)
  where getInd :: [(Var, Int)] -> (Term, Product Expr)
        getInd [] = (mempty, mempty)
        getInd [a] = let r = f a in if r == (toExpr . toTerm $ a) then (toTerm a, mempty) else (mempty, Product r)
        getInd (a@(v, _) : as) = (if isIndFromOthers v then (mempty, Product (f a)) else (toTerm a, mempty)) <> getInd as
        isIndFromOthers v = all (i v) . filter (/= v) . map fst $ rvList
        cntList = filter ((Cnt ==) . getType) . A.toList $ m
        cntExpr = toExpr . foldMap toTerm $ cntList
        rvList = filter ((RV ==) . getType) . A.toList $ m
        getType = varType . fst

collectFromExpr :: IndFunc -> RVReduceFunc -> Expr -> [STVar]
collectFromExpr i f (Expr m) = filter (/= mempty) . A.toListKeys .  getSTSum $  s
    where s = mconcat [STSum (A.singleton stVar (expr * fromIntegral n)) | (t, n) <- A.toList m, let (stVar, expr) = reduce t]
          reduce = indReduce i f

expand :: STVar -> ExpandFunc -> IndFunc -> RVReduceFunc -> [STVar]
expand v@(STVar (Term m)) e i f = collectFromExpr i f s
  where
    s = product [e x ^ a | (x, a) <- A.toList m]

normalize :: STVar -> STVar
normalize (STVar (Term t)) = STVar . Term . A.mapKey subIndex $ t
  where cnt = case index2 . fst . head . A.toList $ t of
              Nothing -> error "Index2 must exist in normalization"
              Just n -> n
        subIndex :: Var -> Var
        subIndex v@Var{index2=Just n} = v{index2=Just (n - cnt)}


data KernelOutput = KernelOutput
  { numberOfEqs :: Int,
    numberOfLevels :: Int,
    levelSize :: [Int]
  }
  deriving (Eq, Show, Generic)

instance Semigroup KernelOutput where
  (<>) (KernelOutput a1 b1 c1) (KernelOutput a2 b2 c2) = KernelOutput (a1 + a2) (b1 + b2) (c1 ++ c2)

instance Monoid KernelOutput where
  mempty = KernelOutput 0 0 []

kernel :: [STVar] -> ExpandFunc -> IndFunc -> RVReduceFunc -> KernelOutput
kernel rootList e i f = go (S.fromList . map normalize $ rootList) S.empty
  where
    go :: Set STVar -> Set STVar -> KernelOutput
    go visitSet seen | S.null visitSet = mempty
                     | otherwise = KernelOutput (length visitList) 1 [length visitList] <> go (neighSet `S.difference` seen') seen'
                    where neighSet = S.fromList . map normalize . concatMap (\v -> expand v e i f) $ visitList
                          visitList = S.toList visitSet
                          seen' = seen <> visitSet

kernelExpr :: Expr -> ExpandFunc -> IndFunc -> RVReduceFunc ->  KernelOutput
kernelExpr expr e i f = kernel (collectFromExpr i f expr) e i f
