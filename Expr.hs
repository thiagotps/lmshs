{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expr
  ( Expr (..),
    Var (..),
    Term (..),
    Amap (..),
    VarType (..),
    exprFromVar,
    mkVar,
    setVarType,
    setVarIndex,
  )
where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

-- NOTE: Amap stands for Algebra Map.
newtype Amap a b = Amap {getAMap :: Map a b} deriving (Eq, Ord)

newtype Expr = Expr (Amap Term Int) deriving (Eq, Ord, Semigroup, Monoid)

newtype Term = Term (Amap Var Int) deriving (Eq, Ord, Semigroup, Monoid)

data VarType = Cnt | RV deriving (Eq, Ord)

data Var = Var
  { varType :: VarType,
    name :: Char,
    index1 :: Maybe Int,
    index2 :: Maybe Int
  }
  deriving (Eq, Ord)

instance (Num b, Ord a, Eq b) => (Semigroup (Amap a b)) where
  (<>) (Amap m) (Amap n) = Amap $ M.filter (/= 0) . M.unionWith (+) m $ n

instance (Num b, Ord a, Eq b) => Monoid (Amap a b) where
  mempty = Amap mempty

instance Num Expr where
  (+) m n = m <> n

  -- TODO: I think that it can be improved using fromListWith
  (*) (Expr (Amap m)) (Expr (Amap n)) = Expr $ mconcat l
    where
      l = [Amap $ M.singleton (x <> y) (a * b) | (x, a) <- M.toList m, (y, b) <- M.toList n]
  abs = error "abs not implemented for Expr"
  signum = error "signum not implemented for Expr"
  fromInteger = Expr . Amap . M.singleton mempty . fromInteger
  negate (Expr (Amap m)) = Expr . Amap . M.map negate $ m

instance Show Expr where
  show (Expr (Amap m)) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- M.toList m]
    where
      showPair cnt mul
        | mul == mempty = show cnt
        | otherwise = show cnt ++ "*" ++ show mul

instance Show Var where
  show (Var _ c i j) = [c] ++ maybe [] show i ++ maybe [] show j

instance Show Term where
  show (Term (Amap m)) = concat [show v ++ "^" ++ show p | (v, p) <- M.toList m]

mkVar :: Char -> Var
mkVar c = Var Cnt c Nothing Nothing

setVarType :: VarType -> Var -> Var
setVarType t v = v {varType = t}

setVarIndex :: (Maybe Int, Maybe Int) -> Var -> Var
setVarIndex (a, b) v = v {index1 = a, index2 = b}

exprFromVar :: Var -> Expr
exprFromVar = exprFromTerm . termFromVar

termFromVar :: Var -> Term
termFromVar v = Term . Amap $ M.singleton v 1

exprFromTerm :: Term -> Expr
exprFromTerm t = Expr . Amap $ M.singleton t 1
