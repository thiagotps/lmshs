-- | 

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Symbolic.Kernel
  ( module Symbolic.Kernel,
    module Symbolic.STVar,
    module Data.Sparse
  )
where

import Symbolic.STVar
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Monoid (Product(Product))
import Data.Semigroup ( Sum(Sum), Sum(getSum) )
import GHC.Generics (Generic)
import Control.Parallel.Strategies

import qualified Symbolic.Amap as A
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import Data.List (partition)
import Control.Applicative (liftA2)
import Debug.Trace (trace)

import Control.Monad
import Control.Monad.RWS
import Control.Monad.ST
import Data.Bifunctor (second, first)
import qualified Data.Vector.Unboxed as V

import Data.Sparse

import qualified Data.List as L

type IndFunc = Int -> Var -> Var -> Bool
type ExpandFunc = Var -> Expr
type ReduceFunc = (Var, Int) -> Maybe Expr
type NumericExpandFunc = Var -> Double
type IniExpandFunc = STVar -> Double

data KernelConfig = KernelConfig
  { indF :: IndFunc,
    expandF :: ExpandFunc,
    reduceF :: ReduceFunc,
    ncpu :: Int
  }

data InnerConfig = InnerConfig
  {
    kernelConfig :: KernelConfig,
    currentLevel :: Int
  }

splitInd :: InnerConfig -> [(Var, Int)] -> (Term, Expr)
splitInd InnerConfig{kernelConfig=KernelConfig{..}, currentLevel=cLevel} rvList =  (term, expr)
  where isIndFromOthers v = all (indF cLevel v) . filter (/= v) . map fst $ rvList
        isReducible v = isJust . reduceF $ (v, 1)
        (l, r) = partition (\(x, _) -> isIndFromOthers x && isReducible x) rvList
        expr = product . map (fromJust . reduceF) $ l
        term = toTerm r

reduceTerm :: InnerConfig -> Term -> STSum
reduceTerm c (Term m) = STSum $ A.singleton (STVar term) (expr * cntExpr)
  where varType' pair = varType $ fst pair
        pairList = A.toList m
        rvList = filter (\x -> varType' x == RV) pairList
        cntList = filter (\x -> varType' x == Cnt) pairList
        (term, expr) = splitInd c rvList
        cntExpr =  toExpr . toTerm $ cntList

term2Double :: (Var -> Double) -> Term -> Double
term2Double f t = foldl (*) 1.0 [f v ^ n | (v, n) <- A.toList . getTerm $ t]

expr2Double :: (Var -> Double) -> Expr -> Double
expr2Double f e = sum [term2Double f t * fromIntegral n | (t, n) <- A.toList . getExpr $ e]

collectFromExpr :: InnerConfig -> Expr -> [(STVar, Expr)]
collectFromExpr config@InnerConfig {kernelConfig=kc} (Expr m) = A.toList . getSTSum $ s
  where
    s =  mconcat [mulSTSum (reduce t) n | (t, n) <- A.toList m]
    reduce = reduceTerm config
    mulSTSum (STSum m) n = STSum . A.map (* fromIntegral n) $ m

expand :: InnerConfig -> STVar -> [(STVar, Expr)]
expand config@InnerConfig{kernelConfig=kc} v@(STVar (Term m))  = collectFromExpr config s
  where
    s = product [expandF kc x ^ a | (x, a) <- A.toList m]

normalize :: STVar -> STVar
normalize (STVar (Term t)) = STVar . Term . A.mapKey subIndex $ t
  where cnt = case index2 . fst . head . A.toList $ t of
              Nothing -> error "Index2 must exist in normalization"
              Just n -> n
        subIndex :: Var -> Var
        subIndex v@Var{index2=Just n} = v{index2=Just (n - cnt)}


type RS r s  = RWS r () s
runRS :: RS r s a -> r -> s -> (s, a)
runRS m r s = let (a, s', _) = runRWS m r s in (s', a)


type SparseSymbolic = [(STVar, [(STVar, Expr)])]

data KernelOutput = KernelOutput
  {
    levelSize :: [Int],
    stateVarList :: [STVar], -- NOTE: This field is redundant
    stateVars :: SparseSymbolic,
    buildEvaluator :: NumericalConfig -> Expr -> (Vector -> Double)
  }

data NumericalMatrices = NumericalMatrices
  {
    matrixA :: Sparse,
    vectorY0 :: Vector,
    vectorB :: Vector
  } deriving (Eq, Show, Generic)

data NumericalConfig = NumericalConfig
  {
    numericExpandF :: NumericExpandFunc,
    iniExpandF :: IniExpandFunc
  }

buildNumMatrices :: NumericalConfig -> SparseSymbolic -> NumericalMatrices
buildNumMatrices NumericalConfig {iniExpandF, numericExpandF} lExpr =
  NumericalMatrices {matrixA, vectorY0, vectorB}
  where
    l = second (second (expr2Double numericExpandF) <$>) <$> lExpr
    stvList = map fst l
    innerLists =  map snd l
    vectorY0 = V.fromList  $ map iniExpandF stvList

    n = length l
    num = M.fromList $ stvList `zip` [0 ..] :: Map STVar Int
    matrixA = Sparse . map (\il -> [(num ! stv, val) | (stv, val) <- il, stv /= mempty]) $ innerLists
    vectorB = V.fromList $ [ fromMaybe 0 (L.lookup mempty il) | il <- innerLists ]



kernel :: KernelConfig -> [STVar] -> KernelOutput
kernel config rootList =
  let (stateVars, levelSize) = runRS go (S.fromList . map normalize $ rootList, S.empty, 1) []
      stateVarList = map fst stateVars
  in KernelOutput {levelSize, stateVarList, stateVars,
                   buildEvaluator = buildEvaluatorInKernel config stateVarList}
  where
    go :: RS (Set STVar, Set STVar, Int) SparseSymbolic [Int]
    go = do
      (visit, seen, level) <- ask

      if S.null visit
        then return mempty
        else do
          let newSeen = seen <> visit

          let transform visitSet = mconcat $ do
                v <- S.toList visitSet
                let r = first normalize <$> expand (InnerConfig config level) v
                let s = S.fromList . filter (/= mempty) . map fst $ r
                return (s S.\\ newSeen, [(v, r)])

          let (newNeigh, partialSTList) = runEval $ do
                let l = divide (ncpu config) visit
                (a, b) <- mconcat <$> mapM (rpar . transform) l
                return (a, b)

          modify (partialSTList <>)
          (length visit :) <$> local (const (newNeigh, newSeen, level + 1)) go


divide :: (Ord a) => Int -> Set a -> [Set a]
divide n s =  f (intLog2 n)
  where f n | n == 0 = [s]
            | otherwise = do
                ss <- f (n - 1)
                let (a,b) = S.splitAt (length ss `div` 2) ss
                [a,b]
        intLog2 = floor . logBase 2 . fromIntegral


kernelExpr :: KernelConfig -> Expr -> KernelOutput
kernelExpr config expr = kernel config $ fst <$> collectFromExpr (InnerConfig config 1) expr

buildEvaluatorInKernel :: KernelConfig -> [STVar] -> NumericalConfig ->  Expr -> (Vector -> Double)
buildEvaluatorInKernel config stateVarList NumericalConfig{numericExpandF} expr = eval
  where
        (iniSTVlist, vec) = let l = second (expr2Double numericExpandF) <$> collectFromExpr (InnerConfig config 1) expr
                                in (normalize . fst <$> l, V.fromList $ snd <$> l)
        stvEnum = M.fromList (stateVarList `zip` [0..]) :: Map STVar Int
        idxVec = V.fromList $ map (stvEnum ! ) iniSTVlist
        eval y = let n = V.length idxVec
                     sy :: Vector
                     sy = V.generate n (\i -> let ii = idxVec V.! i in (y V.! ii))
                     in vec !.! sy
