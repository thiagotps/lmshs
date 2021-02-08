module Main where

import Symbolic.Expr
import Symbolic.STVar

a :: Int -> Var
a n = setVarIndex (Just n, Nothing) . mkVar $ 'a'

u :: Int -> Var
u n = setVarType RV . setVarIndex (Nothing, Just n) . mkVar $ 'u'

xk n = sum [exprFromVar (a i) * exprFromVar (u (- i + n)) | i <- [0, 1]]

v1k n =
  exprFromVar
    (mkVar 'v')
      { varType = RV,
        index1 = Just 1,
        index2 = Just n
      }

isInd :: IndFunc
isInd x y = go x y || go y x
  where
    go Var {name = 'u', index2 = Just n} Var {name = 'v', index2 = Just m} = n >= m
    go Var {name = 'u', index2 = Just n} Var {name = 'u', index2 = Just m} = n /= m
    go Var {name = 'n'} _ = True
    go _ _ = False

defaultVar = Var {name = ' ', varType = Cnt, index1 = Nothing, index2 = Nothing}

micro :: Expr
micro = exprFromVar defaultVar{name = 'µ'}
nk n = exprFromVar defaultVar{varType=RV, name='n', index2=Just n}

expandFunc :: ExpandFunc
expandFunc Var{name='v', index2 = Just n} = (1 - micro*(xk n)^2)*(v1k n) + micro*(nk n)*(xk n)
expandFunc v = exprFromVar v

reduce :: RVReduceFunc
reduce (Var{name='u'}, n) | n == 1 = 0
                          | otherwise = toExpr defaultVar{name='γ', index1=Just n}
reduce (v, n) = toExpr v ^ n

expr = (v1k 0)^2 * (xk 0) ^ 2
stList = collectFromExpr isInd reduce  expr

-- expr2 = (v1k 0 ^ 2) * (toExpr (a 1)) ^ 2 * (toExpr (u (-1))) ^2
-- res2 = indReduce isInd reduce (toTerm . A.toList . getExpr $ expr2)


main :: IO ()
main = do
  print stList
  print expr
