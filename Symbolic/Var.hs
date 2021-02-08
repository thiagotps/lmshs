-- |

module Symbolic.Var (Var(..), VarType(..)) where


data VarType = Cnt | RV deriving (Eq, Ord)

data Var = Var
  { varType :: VarType,
    name :: Char,
    index1 :: Maybe Int,
    index2 :: Maybe Int
  }
  deriving (Eq, Ord)

instance Show Var where
  show (Var _ c i j) = [c] ++ maybe [] show i ++ maybe [] show j
