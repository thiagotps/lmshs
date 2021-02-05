module Expr
  ( Expr,
    Var (..),
    Term (..),
    exprFromVar,
    mkVar,
    setVarType,
    setVarIndex,
  )
where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

data Expr = Expr (Map Term Int) Int deriving (Eq)

newtype Term = Term (Map Var Int) deriving (Eq, Ord)

data VarType = Cnt | RV deriving (Eq, Ord)

data Var = Var
  { varType :: VarType,
    name :: Char,
    index1 :: Maybe Int,
    index2 :: Maybe Int
  }
  deriving (Eq, Ord)

instance Semigroup Term where
  (<>) (Term m1) (Term m2) = Term $ M.unionWith (+) m1 m2

instance Monoid Term where
  mempty = Term M.empty

instance Num Expr where
  (+) (Expr m a) (Expr n b) = Expr q (a + b)
    where q = M.filter (/=0) $ M.unionWith (+) m n

  -- TODO: This can receive a better implementation
  (*) (Expr m a) (Expr n b) = Expr mn 0 + scale m b + scale n a + Expr M.empty (a * b)
    where
      lm = M.toList m
      ln = M.toList n
      mn = M.fromListWith (+) [(x <> y, a * b) | (x, a) <- lm, (y, b) <- ln]
      scale :: Map Term Int -> Int -> Expr
      scale m 0 = 0
      scale m 1 = Expr m 0
      scale m a = Expr (M.map (* a) m) 0

  abs = error "abs is not implemented"
  signum = error "signum is not implemented"
  fromInteger n = Expr M.empty (fromInteger n)
  negate (Expr m a) = Expr (M.map negate m) (- a)

instance Show Expr where
  show (Expr m a) = L.intercalate " + " $ [show cnt ++ "*" ++ show mul | (mul, cnt) <- M.toList m] ++ [show a]

instance Show Var where
  show (Var _ c i j) = [c] ++ maybe [] show i ++ maybe [] show j

instance Show Term where
  show (Term m1) = concat [show v ++ "^" ++ show p | (v, p) <- M.toList m1]

mkVar :: Char -> Var
mkVar c = Var Cnt c Nothing Nothing

setVarType :: VarType -> Var -> Var
setVarType t v = v {varType = t}

setVarIndex :: (Maybe Int, Maybe Int) -> Var -> Var
setVarIndex (a, b) v = v {index1 = a, index2 = b}

exprFromVar :: Var -> Expr
exprFromVar = exprFromTerm . termFromVar

termFromVar :: Var -> Term
termFromVar v = Term $ M.singleton v 1

exprFromTerm :: Term -> Expr
exprFromTerm t = Expr (M.singleton t 1) 0
