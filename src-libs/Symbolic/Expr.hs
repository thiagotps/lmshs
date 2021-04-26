{-# LANGUAGE FunctionalDependencies #-}
module Symbolic.Expr
  ( module Symbolic.Expr,
    module Symbolic.Term
  )
where

import qualified Data.List as L
import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A
import Symbolic.Term
import Data.Map (Map)
import qualified Data.Map as M

newtype Expr a = Expr {getExpr :: Amap Term a} deriving (Eq, Ord, Semigroup, Monoid)

type ExprInt = Expr Int
type ExprDouble = Expr Double

instance (Num a, Eq a) => Num (Expr a) where
  (+) m n = m <> n

  -- TODO: I think that it can be improved using fromListWith
  (*) (Expr m) (Expr n) = Expr $ mconcat l
    where
      l = [A.singleton (x <> y) (a * b) | (x, a) <- A.toList m, (y, b) <- A.toList n]
  abs = error "abs not implemented for Expr"
  signum = error "signum not implemented for Expr"
  fromInteger = Expr . A.singleton mempty . fromInteger
  negate (Expr m) = Expr . A.map negate $  m

instance (Num a, Eq a, Show a) => Show (Expr a) where
  show (Expr m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul
        | mul == mempty = show cnt
        | cnt == 1 = show mul
        | otherwise = show cnt ++ "*" ++ show mul

class IsExpr a b  where
  toExpr :: a -> Expr b

instance Num a => IsExpr (Expr a) a where
  toExpr = id

instance (Num b, Eq b) => IsExpr Term b where
  toExpr t = Expr (A.singleton t 1)

instance IsExpr (Term, Int) Int where
  toExpr (t, n) = Expr (A.singleton t n)

instance (Num b, Eq b) => IsExpr Var b where
  toExpr = toExpr . toTerm
