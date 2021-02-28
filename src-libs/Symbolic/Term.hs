
-- |
module Symbolic.Term
  ( module Symbolic.Term,
    module Symbolic.Var
  )
where

import Data.Hashable (Hashable)

import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A
import Symbolic.Var

newtype Term = Term {getTerm :: Amap Var Int} deriving (Eq, Ord, Semigroup, Monoid, Hashable)

instance Show Term where
  show (Term m) = concat [fromPair v p | (v, p) <- A.toList m]
    where fromPair v p | p == 1 = show v
                       | otherwise = show v ++ "^" ++ show p

class IsTerm a where
  toTerm :: a -> Term

instance IsTerm Term where
  toTerm = id

instance IsTerm Var where
  toTerm v = Term (A.singleton v 1)

instance IsTerm (Var, Int) where
  toTerm (v, n) = Term (A.singleton v n)

instance IsTerm [(Var, Int)] where
  toTerm = foldMap toTerm
