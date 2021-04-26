-- |
module Model.Classic where

import qualified Symbolic.Amap as A
import Symbolic.Kernel

a :: Int -> Var
a n = defaultVar {name = 'a', index1 = Just n}

u :: Int -> Var
u n = defaultVar {name = 'u', index2 = Just n, varType = RV}

type DataLenght = Int

xBuilder :: DataLenght -> Int -> ExprInt
xBuilder dataLenght = x
  where
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

type FilterLenght = Int

innBuilder :: FilterLenght -> DataLenght -> Int -> ExprInt
innBuilder filterLenght dataLenght = innSum
  where
    innSum :: Int -> ExprInt
    innSum n = sum [x (n - j) * toExpr (vik j n) | j <- [0 .. filterLenght - 1]]
    x = xBuilder dataLenght

expandFuncBuilder :: FilterLenght -> DataLenght -> ExpandFunc
expandFuncBuilder filterLenght dataLenght = expandFunc
  where
    expandFunc :: ExpandFunc
    expandFunc Var {name = 'v', index1 = Just i, index2 = Just n1} =
      toExpr (vik i n) - toExpr micro * x (n - i) * inn
        + toExpr micro * nu n * x (n - i)
      where
        n = n1 - 1
        inn = innSum n
    expandFunc v = toExpr v
    innSum = innBuilder filterLenght dataLenght
    x = xBuilder dataLenght

reduce :: ReduceFunc
reduce (Var {name = 'u'}, n)
  | odd n = Just 0
  | otherwise = Just $ toExpr defaultVar {name = 'γ', index1 = Just n}
reduce (Var {name = 'n'}, n)
  | odd n = Just 0
  | otherwise = Just $ toExpr defaultVar {name = 'σ'} ^ n
reduce _ = Nothing

buildExpr :: FilterLenght -> DataLenght -> ExprInt
buildExpr filterLenght dataLenght = inn ^ 2
  where
    inn = innSum 0
    innSum = innBuilder filterLenght dataLenght

buildConfig :: FilterLenght -> DataLenght -> KernelConfig
buildConfig filterLenght dataLenght =
  KernelConfig{indF=isInd, reduceF=reduce, expandF=expandFunc}
  where
    expandFunc = expandFuncBuilder filterLenght dataLenght

runModel :: FilterLenght -> DataLenght -> KernelOutput
runModel filterLenght dataLenght = kernelExpr config finalExpr
  where
    finalExpr = buildExpr filterLenght dataLenght
    config = buildConfig filterLenght dataLenght
