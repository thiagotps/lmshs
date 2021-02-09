-- |
module Symbolic.STVar
  (
    STSum (..),
    STVar (..),
    -- expand,
    -- kernel,
    indReduce,
    collectFromExpr,
    IndFunc,
    ExpandFunc,
    RVReduceFunc,
    kernel,
    expand
  )
where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Symbolic.Expr
import Symbolic.Term
import qualified Symbolic.Term as T
import Symbolic.Var
import qualified Symbolic.Amap as A
import Data.Monoid (Product(Product))
import Debug.Trace (trace)

newtype STSum = STSum {getSTSum :: Amap STVar Expr} deriving (Eq, Ord, Semigroup, Monoid)

newtype STVar = STVar {getSTVar :: Term} deriving (Eq, Ord, Semigroup, Monoid)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul | mul == mempty = show cnt
                       | otherwise = "(" ++ show cnt ++ ")" ++ "*" ++ show mul

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


kernel :: [STVar] -> ExpandFunc -> IndFunc -> RVReduceFunc -> Int
kernel rootList e i f = length $ go (map id rootList) S.empty
  where
    go :: [STVar] -> Set STVar -> [STVar]
    go [] _ = []
    go (vRaw : vs) seen =
      let seen' = S.insert v seen
          v = normalize vRaw
          neigh = map id (expand v e i f)
       in if v `S.member` seen
            then go vs seen
            else v : go (vs ++ neigh) seen'
