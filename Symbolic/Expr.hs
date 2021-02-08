
module Symbolic.Expr
  ( Expr (..),
    Var (..),
    Term (..),
    Amap (..),
    VarType (..),
    IsExpr,
    toExpr,
    exprFromVar,
    termFromVar,
    termFromList,
    exprFromTerm,
    mkVar,
    setVarType,
    setVarIndex,
  )
where

import qualified Data.List as L
import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A
import Symbolic.Var
import Symbolic.Term
import Data.Map (Map)
import qualified Data.Map as M

newtype Expr = Expr {getExpr :: Amap Term Int} deriving (Eq, Ord, Semigroup, Monoid)

instance Num Expr where
  (+) m n = m <> n

  -- TODO: I think that it can be improved using fromListWith
  (*) (Expr m) (Expr n) = Expr $ mconcat l
    where
      l = [A.singleton (x <> y) (a * b) | (x, a) <- A.toList m, (y, b) <- A.toList n]
  abs = error "abs not implemented for Expr"
  signum = error "signum not implemented for Expr"
  fromInteger = Expr . A.singleton mempty . fromInteger
  negate (Expr m) = Expr . A.map negate $  m

instance Show Expr where
  show (Expr m) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- A.toList m]
    where
      showPair cnt mul
        | mul == mempty = show cnt
        | otherwise = show cnt ++ "*" ++ show mul

class IsExpr a where
  toExpr :: a -> Expr

instance IsExpr Expr where
  toExpr = id

instance IsExpr Term where
  toExpr t = Expr (A.singleton t 1)

instance IsExpr (Term, Int) where
  toExpr (t, n) = Expr (A.singleton t n)

instance IsExpr Var where
  toExpr = toExpr . toTerm


termFromList :: [(Var, Int)] -> Term
termFromList l = Term $ mconcat [A.singleton x n | (x,n) <- l]

mkVar :: Char -> Var
mkVar c = Var Cnt c Nothing Nothing

setVarType :: VarType -> Var -> Var
setVarType t v = v {varType = t}

setVarIndex :: (Maybe Int, Maybe Int) -> Var -> Var
setVarIndex (a, b) v = v {index1 = a, index2 = b}

exprFromVar :: Var -> Expr
exprFromVar = exprFromTerm . termFromVar

termFromVar :: Var -> Term
termFromVar v = Term (A.singleton v 1)

exprFromTerm :: Term -> Expr
exprFromTerm t = Expr (A.singleton t 1)
