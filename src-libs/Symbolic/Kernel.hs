-- | 

{-# LANGUAGE RecordWildCards #-}
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
import Data.Maybe (isJust, fromJust, isNothing)
import Data.List (partition)
import Control.Applicative (liftA2)
import Debug.Trace (trace)


type IndFunc = Var -> Var -> Bool
type ExpandFunc = Var -> Expr
type ReduceFunc = (Var, Int) -> Maybe Expr

data KernelConfig = KernelConfig
  { indF :: IndFunc,
    expandF :: ExpandFunc,
    reduceF :: ReduceFunc
  }

splitInd :: KernelConfig -> [(Var, Int)] -> (Term, Expr)
splitInd KernelConfig{..} rvList =  (term, expr)
  where isIndFromOthers v = all (indF v) . filter (/= v) . map fst $ rvList
        isReducible v = isJust . reduceF $ (v, 1)
        (l, r) = partition (\(x, _) -> isIndFromOthers x && isReducible x) rvList
        expr = product . map (fromJust . reduceF) $ l
        term = toTerm r

reduceTerm :: KernelConfig -> Term -> STSum
reduceTerm c (Term m) = STSum $ A.singleton (STVar term) (expr * cntExpr)
  where varType' pair = varType $ fst pair
        pairList = A.toList m
        rvList = filter (\x -> varType' x == RV) pairList
        cntList = filter (\x -> varType' x == Cnt) pairList
        (term, expr) = splitInd c rvList
        cntExpr =  toExpr . toTerm $ rvList

collectFromExpr :: KernelConfig -> Expr -> [STVar]
collectFromExpr config  (Expr m) = filter (/= mempty) . A.toListKeys .  getSTSum $  s
    where s = mconcat [mulSTSum (reduce t) n | (t, n) <- A.toList m]
          reduce = reduceTerm config
          mulSTSum (STSum m) n = STSum . A.map (* fromIntegral n) $ m

expand :: KernelConfig -> STVar -> [STVar]
expand config@KernelConfig{..} v@(STVar (Term m))  = collectFromExpr config s
  where
    s = product [expandF x ^ a | (x, a) <- A.toList m]

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

kernel :: KernelConfig -> [STVar]   -> KernelOutput
kernel config rootList  = go (S.fromList . map normalize $ rootList) S.empty
  where
    go :: Set STVar -> Set STVar -> KernelOutput
    go visitSet seen | S.null visitSet = mempty
                     | otherwise = KernelOutput (length visitList) 1 [length visitList] <> go (neighSet `S.difference` seen') seen'
                    where neighSet = S.fromList . map normalize . concatMap (expand config) $ visitList
                          visitList = S.toList visitSet
                          seen' = seen <> visitSet

kernelExpr :: KernelConfig -> Expr -> KernelOutput
kernelExpr = liftA2 (.) kernel collectFromExpr
