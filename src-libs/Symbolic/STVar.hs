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

newtype STSum = STSum {getSTSum :: Amap STVar Expr} deriving (Eq, Ord, Semigroup, Monoid)

newtype STVar = STVar {getSTVar :: Term} deriving (Eq, Ord, Semigroup, Monoid)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul | mul == mempty = show cnt
                       | otherwise = "(" ++ show cnt ++ ")" ++ "*" ++ show mul
