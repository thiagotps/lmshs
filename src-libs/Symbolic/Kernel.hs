-- | 

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Symbolic.Kernel
  ( module Symbolic.Kernel,
    module Symbolic.STVar,
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
import Data.Bifunctor (second, first)
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as N
import Numeric.LinearAlgebra.Data (build, Vector)


type IndFunc = Var -> Var -> Bool
type ExpandFunc = Var -> Expr
type ReduceFunc = (Var, Int) -> Maybe Expr
type NumericExpandFunc = Var -> Double

data KernelConfig = KernelConfig
  { indF :: IndFunc,
    expandF :: ExpandFunc,
    reduceF :: ReduceFunc,
    numericExpandF :: NumericExpandFunc
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

term2Double :: (Var -> Double) -> Term -> Double
term2Double f t = sum [f v ^ n | (v, n) <- A.toList . getTerm $ t]

expr2Double :: (Var -> Double) -> Expr -> Double
expr2Double f e = sum [term2Double f t * fromIntegral n | (t, n) <- A.toList . getExpr $ e]

collectFromExpr :: KernelConfig -> Expr -> [(STVar, Double)]
collectFromExpr config@KernelConfig {numericExpandF = f} (Expr m) = second (expr2Double f) <$> (A.toList . getSTSum $ s)
  where
    s = mconcat [mulSTSum (reduce t) n | (t, n) <- A.toList m]
    reduce = reduceTerm config
    mulSTSum (STSum m) n = STSum . A.map (* fromIntegral n) $ m

expand :: KernelConfig -> STVar -> [(STVar, Double)]
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


type RS r s  = RWS r () s
runRS :: RS r s a -> r -> s -> (s, a)
runRS m r s = let (a, s', _) = runRWS m r s in (s', a)

data KernelOutput = KernelOutput
  {
    levelSize :: [Int],
    stateVarList :: [STVar],
    matrixA :: Matrix Double,
    vectorB :: Vector Double
  } deriving (Eq, Show, Generic)

buildY0Vector :: [STVar] -> Vector Double
buildY0Vector = undefined

-- TODO: Improve this implementation
buildMatrix :: Map STVar (Map STVar Double) -> KernelOutput
buildMatrix m =
  KernelOutput
    { levelSize = [],
      stateVarList = vars,
      matrixA = build (n, n) f,
      vectorB = build n g
    }
  where
    vars = M.keys m
    n = length vars
    vars2num = M.fromList $ zip vars [0 ..]
    num2vars = M.fromList $ zip [0 ..] vars
    f i j =
      let va = num2vars ! i
          vb = num2vars ! j
          maybeDouble = (m ! va) !? vb
       in fromMaybe 0 maybeDouble
    g i =
      let va = num2vars ! i
          maybeDouble = (m ! va) !? mempty
       in fromMaybe 0 maybeDouble


kernel :: KernelConfig -> [STVar] -> KernelOutput
kernel config rootList =
  let (s, l) = runRS go (S.fromList . map normalize $ rootList) M.empty
      out = buildMatrix s
   in out {levelSize = l}
  where
    go :: RS (Set STVar) (Map STVar (Map STVar Double)) [Int]
    go = do
      visit <- ask
      seen <- gets M.keysSet

      if S.null visit
        then return mempty
        else do
          let newSeen = seen <> visit

          let transform visitSet = mconcat $ do
                v <- S.toList visitSet
                let r = first normalize <$> expand config v
                let s = S.fromList . filter (/= mempty) . map fst $ r
                let m = M.fromList r
                return (s S.\\ newSeen, M.singleton v m)

          let (newNeigh, partialStMap) = runEval $ do
                let l = divide 4 visit
                (a, b) <- mconcat <$> mapM (rpar . transform) l
                return (a, b)

          modify (partialStMap <>)
          (length visit :) <$> local (const newNeigh) go


divide :: (Ord a) => Int -> Set a -> [Set a]
divide n s =  f (intLog2 n)
  where f n | n == 0 = [s]
            | otherwise = do
                ss <- f (n - 1)
                let (a,b) = S.splitAt (length ss `div` 2) ss
                [a,b]
        intLog2 = floor . logBase 2 . fromIntegral


kernelExpr :: KernelConfig -> Expr -> KernelOutput
kernelExpr config expr = kernel config $ fst <$> collectFromExpr config expr
