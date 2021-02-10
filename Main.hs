module Main where

import Symbolic.Expr
import Symbolic.Term
import Symbolic.STVar
import Symbolic.Var
import qualified Symbolic.Amap as A
import Debug.Trace (trace)

a :: Int -> Var
a n = defaultVar{name='a', index1=Just n}
u :: Int -> Var
u n = defaultVar{name='u', index2=Just n, varType=RV}

filterLenght = 7 :: Int
dataLenght = 1 :: Int
x n = sum [toExpr (a i) * toExpr (u (n - i)) | i <- [0..(dataLenght - 1)]]

vik i n =
  defaultVar
      { varType = RV,
        name='v',
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

micro :: Expr
micro = toExpr defaultVar{name = 'µ'}
nu n = toExpr defaultVar{varType=RV, name='n', index2=Just n}

expandFunc :: ExpandFunc
-- expandFunc Var{name='v', index2 = Just n1} = (1 - micro*(x n)^2)*(toExpr . (vik 0) $ n) + micro*(nu n)*(x n)
--   where n = n1 - 1
expandFunc Var{name='v', index1 = Just i, index2 = Just n1} = (toExpr (vik i n)) - micro*(x (n - i)) * inn + micro*(nu n)*(x (n - i))
  where n = n1 - 1
        inn = sum [x (n - j)*(toExpr (vik j n)) | j <- [0..filterLenght - 1]]
expandFunc v = toExpr v

reduce :: RVReduceFunc
reduce (Var{name='u'}, n) | odd n = 0
                          | otherwise = toExpr defaultVar{name='γ', index1=Just n}
reduce (Var{name='n'}, n) | odd n = 0
                          | otherwise = toExpr defaultVar{name='σ'} ^ n
reduce (v, n) = toExpr v ^ n

expr = (toExpr . vik 1 $ 0)^2 * (x 0) ^ 2
-- stList = collectFromExpr isInd reduce  expr
-- numberOfEqs = kernel stList expandFunc isInd reduce

-- expr2 = foldMap toTerm [(v1k 0, 2 :: Int), (a 1, 2), (u (-1),  2)]
-- -- expr2 = (v1k 0 ^ 2) * (toExpr (a 1)) ^ 2 * (toExpr (u (-1))) ^2
-- res2 = indReduce isInd reduce expr2

-- expr3 = foldMap toTerm [(a 0, 2 :: Int), (v1k 0, 2), (u 0, 2)]
-- res3 = indReduce isInd reduce expr3

-- res4 = collectFromExpr isInd reduce (expandFunc (v1k 1) ^ 2)

finalExpr = (inn) ^ 2
  where inn = sum [(x (0 - j)) * (toExpr (vik j 0)) | j <- [0..filterLenght - 1]]
finalNumberEqs = kernel (collectFromExpr isInd reduce finalExpr) expandFunc isInd reduce

collectFromExpr2 :: IndFunc -> RVReduceFunc -> Expr -> STSum
collectFromExpr2 i f (Expr m) = s
    where s = mconcat [STSum (A.singleton stVar (expr * fromIntegral n)) | (t, n) <- A.toList m, let (stVar, expr) = reduce t]
          reduce = indReduce i f

expand2 :: STVar -> ExpandFunc -> IndFunc -> RVReduceFunc -> STSum
expand2 (STVar (Term m)) e i f = collectFromExpr2 i f (trace ("teste: " ++ show s ++ "\n") s)
  where
    s = product [e x ^ a | (x, a) <- A.toList m]

main :: IO ()
main = do
  print "Ola"
  -- print res2
  -- print res3
  -- print res4
  -- print stList
  -- print numberOfEqs
  -- print expr
  -- print finalExpr
  print finalNumberEqs
  -- print (expand2 (STVar $ foldMap toTerm [(vik 0 1, 2 :: Int), (u 0, 4)])  expandFunc isInd reduce  )
  -- print (expandFunc (vik 0 1) ^ 2 * toExpr (u 0) ^ 4 )
  -- print (collectFromExpr2 isInd reduce finalExpr)
  -- print (expand2 testStVar  expandFunc isInd reduce  )
  -- print $ indReduce isInd reduce testTerm
  -- print $ expand2 testStVar expandFunc isInd reduce
  -- putStrLn $ "First = " ++ show (v0k1Expr)
  -- putStrLn $ "Second = " ++ show (v0k1Expr ^2)
  -- putStrLn $ "Third = " ++ show (v0k1Expr ^2 * (toExpr (u 0)) ^ 4)
  -- where testTerm = foldMap toTerm [(a 0, 1 :: Int), (defaultVar{name = 'µ'}, 1), (u 0, 6), (vik 0 0, 2)]
  --       testStVar = STVar $ foldMap toTerm [(vik 0 1, 2 :: Int), (u 0, 4)]
  --       v0k1Expr = expandFunc (vik 0 1)

  -- print ((x - y)^3)
  -- where x = toExpr defaultVar{name='x'}
  --       y = toExpr defaultVar{name='y'}
