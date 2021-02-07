{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
module STVar
  (
    STSum (..),
    STVar (..),
    expand,
    kernel,
    indReduce,
    collectFromExpr
  )
where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Expr

newtype STSum = STSum (Amap (Maybe STVar) Expr) deriving (Eq, Ord, Semigroup, Monoid)

newtype STVar = STVar Term deriving (Eq, Ord)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum (Amap m)) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- M.toList m]
    where
      showPair cnt mul = case mul of
        Just m -> show cnt ++ "*" ++ show m
        Nothing -> show cnt

indReduce :: (Var -> Var -> Bool) -> Term -> STVar
indReduce i (Term (Amap vm)) = STVar . Term . Amap . M.fromList . getIndList $ rvList
  where
    rvList = filter ((RV ==) . varType . fst) varList
    varList = M.toList vm
    getIndList [] = []
    getIndList (a : as) = if all (i (fst a) . fst) as then a : getIndList as else getIndList as

collectFromExpr :: (Var -> Var -> Bool) -> Expr -> [STVar]
collectFromExpr i expr = S.toList stVarSet
    where termSet = let (Expr (Amap sm)) = expr in M.keysSet sm
          stVarSet = S.map (indReduce i) termSet

expand :: STVar -> (Var -> Expr) -> (Var -> Var -> Bool) -> [STVar]
expand (STVar (Term (Amap m))) e i = collectFromExpr i s
  where
    s = sum [e x ^ a | (x, a) <- M.toList m]

kernel :: STVar -> (Var -> Expr) -> (Var -> Var -> Bool) -> Int
kernel root e i = length (go [root] S.empty)
  where
    go :: [STVar] -> Set STVar -> [STVar]
    go [] _ = []
    go (v : vs) seen =
      let seen' = S.insert v seen
          neigh = expand v e i
       in if v `S.member` seen
            then go vs seen
            else v : go (vs ++ neigh) seen'
