-- |
module Symbolic.STVar
  ( module Symbolic.STVar,
    module Symbolic.Expr
  )
where

import qualified Data.List as L
import Symbolic.Expr
import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A

newtype STSum a = STSum {getSTSum :: Amap STVar a} deriving (Eq, Ord, Semigroup, Monoid)

type STSumExpr = STSum ExprInt
type STSumDouble = STSum Double

newtype STVar = STVar {getSTVar :: Term} deriving (Eq, Ord, Semigroup, Monoid)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance (Show a) => Show (STSum a) where
  show (STSum m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul | mul == mempty = show cnt
                       | otherwise = "(" ++ show cnt ++ ")" ++ "*" ++ show mul
