
-- |
module Symbolic.Term
  ( Term (..),
    IsTerm,
    toTerm
  )
where

import Symbolic.Amap (Amap)
import qualified Symbolic.Amap as A
import Symbolic.Var

newtype Term = Term {getTerm :: Amap Var Int} deriving (Eq, Ord, Semigroup, Monoid)

instance Show Term where
  show (Term m) = concat [show v ++ "^" ++ show p | (v, p) <- A.toList m]

class IsTerm a where
  toTerm :: a -> Term

instance IsTerm Term where
  toTerm = id

instance IsTerm Var where
  toTerm v = Term (A.singleton v 1)

instance IsTerm (Var, Int) where
  toTerm (v, n) = Term (A.singleton v n)
