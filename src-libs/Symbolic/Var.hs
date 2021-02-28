-- |

{-# LANGUAGE DeriveAnyClass #-}
module Symbolic.Var (Var(..), VarType(..), defaultVar) where
import qualified Data.List as L

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data VarType = Cnt | RV deriving (Eq, Enum, Ord, Generic, Hashable)


data Var = Var
  { varType :: VarType,
    name :: Char,
    index1 :: Maybe Int,
    index2 :: Maybe Int
  }
  deriving (Eq, Ord, Generic, Hashable)

instance Show Var where
  show (Var _ c i j) = c : L.intercalate "," (filter (not . null) [maybe [] show i , case j of
                                                                      Nothing -> []
                                                                      Just jj -> "k" ++ showWithSign jj])
                       where showWithSign n = case compare n 0 of
                                                LT -> show n
                                                EQ -> ""
                                                GT -> "+" ++ show n

defaultVar = Var {name = ' ', varType = Cnt, index1 = Nothing, index2 = Nothing}
