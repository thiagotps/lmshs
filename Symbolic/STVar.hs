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
    RVReduceFunc
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

newtype STSum = STSum {getSTSum :: Amap STVar Expr} deriving (Eq, Ord, Semigroup, Monoid)

newtype STVar = STVar {getSTVar :: Term} deriving (Eq, Ord, Semigroup, Monoid)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul | mul == mempty = show cnt
                       | otherwise = show cnt ++ "*" ++ show mul

type IndFunc = Var -> Var -> Bool
type ExpandFunc = Var -> Expr
type RVReduceFunc = (Var, Int) -> Expr

indReduce :: IndFunc -> RVReduceFunc -> Term -> (STVar, Expr)
indReduce i f (Term m) = let (term, expr) = getInd rvList
                         in  (STVar term, cntExpr * expr)
  where getInd :: [(Var, Int)] -> (Term, Expr)
        getInd [] = (mempty, mempty)
        -- NOTE: Zuado aqui!!!
        getInd (a@(v, _) : as) = (if all (i v . fst) (filter (/= a) rvList) then (mempty, f a) else (toTerm a, mempty) ) <> getInd as
        cntList = filter ((Cnt ==) . varType . fst) . A.toList $ m
        cntExpr = toExpr . foldMap toTerm $ cntList
        rvList = filter ((RV ==) . varType . fst) . A.toList $ m

collectFromExpr :: IndFunc -> RVReduceFunc -> Expr -> [STVar]
collectFromExpr i f (Expr m) = A.toListKeys .  getSTSum $ s
    where s = mconcat [STSum (A.singleton stVar (expr * fromIntegral n)) | (t, n) <- A.toList m, let (stVar, expr) = reduce t]
          reduce = indReduce i f

-- expand :: STVar -> (Var -> Expr) -> (Var -> Var -> Bool) -> [STVar]
-- expand (STVar (Term (Amap m))) e i = collectFromExpr i s
--   where
--     s = sum [e x ^ a | (x, a) <- M.toList m]

-- kernel :: STVar -> (Var -> Expr) -> (Var -> Var -> Bool) -> Int
-- kernel root e i = length (go [root] S.empty)
--   where
--     go :: [STVar] -> Set STVar -> [STVar]
--     go [] _ = []
--     go (v : vs) seen =
--       let seen' = S.insert v seen
--           neigh = expand v e i
--        in if v `S.member` seen
--             then go vs seen
--             else v : go (vs ++ neigh) seen'
