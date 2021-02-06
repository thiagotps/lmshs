{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module STVar  where
import Expr
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

newtype STSum = STSum (Amap (Maybe STVar) Expr) deriving (Eq, Ord, Semigroup, Monoid)
newtype STVar = STVar Term deriving (Eq, Ord)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum (Amap m)) = L.intercalate " + " $ [showPair cnt mul | (mul, cnt) <- M.toList m]
    where showPair cnt mul = case mul of
                               Just m -> show cnt ++ "*" ++ show m
                               Nothing -> show cnt

-- -- expand :: STVar -> (Var -> Expr) -> STSum
-- -- expand (STVar (Term m)) f = mempty
-- --   -- where

-- expand :: STVar -> (Var -> Expr) -> [STVar]
-- expand (STVar (Term m)) f = sum [()]
