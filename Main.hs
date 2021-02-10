module Main where

import Debug.Trace (trace)
import qualified Symbolic.Amap as A
import Symbolic.Expr
import Symbolic.STVar
import Symbolic.Term
import Symbolic.Var

filterLenght = 6 :: Int

dataLenght = 1 :: Int

a :: Int -> Var
a n = defaultVar {name = 'a', index1 = Just n}

u :: Int -> Var
u n = defaultVar {name = 'u', index2 = Just n, varType = RV}

x n = sum [toExpr (a i) * toExpr (u (n - i)) | i <- [0 .. (dataLenght - 1)]]

vik i n =
  defaultVar
    { varType = RV,
      name = 'v',
      index1 = Just i,
      index2 = Just n
    }

isInd :: IndFunc
isInd x y = go x y || go y x
  where
    go Var {name = 'u', index2 = Just n} Var {name = 'v', index2 = Just m} = n >= m
    go Var {name = 'u', index2 = Just n} Var {name = 'u', index2 = Just m} = n /= m
    go Var {name = 'n'} _ = True
    go _ _ = False

micro :: Var
micro = defaultVar {name = 'µ'}

nu n = toExpr defaultVar {varType = RV, name = 'n', index2 = Just n}

innSum :: Int -> Expr
innSum n = sum [x (n - j) * toExpr (vik j n) | j <- [0 .. filterLenght - 1]]

expandFunc :: ExpandFunc
expandFunc Var {name = 'v', index1 = Just i, index2 = Just n1} =
  toExpr (vik i n) - toExpr micro * x (n - i) * inn
    + toExpr micro * nu n * x (n - i)
  where
    n = n1 - 1
    inn = innSum n
expandFunc v = toExpr v

reduce :: RVReduceFunc
reduce (Var {name = 'u'}, n)
  | odd n = 0
  | otherwise = toExpr defaultVar {name = 'γ', index1 = Just n}
reduce (Var {name = 'n'}, n)
  | odd n = 0
  | otherwise = toExpr defaultVar {name = 'σ'} ^ n
reduce (v, n) = toExpr v ^ n

finalExpr = inn ^ 2
  where
    inn = innSum 0

finalNumberEqs = kernel (collectFromExpr isInd reduce finalExpr) expandFunc isInd reduce

main :: IO ()
main = do
  print finalNumberEqs
