-- |
{-# LANGUAGE RecordWildCards #-}
module Model.Classic where

import qualified Symbolic.Amap as A
import Symbolic.Kernel

a :: Int -> Var
a n = defaultVar {name = 'a', index1 = Just n}

u :: Int -> Var
u n = defaultVar {name = 'u', index2 = Just n, varType = RV}

type DataLenght = Int

xBuilder :: DataLenght -> Int -> Expr
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

innBuilder :: FilterLenght -> DataLenght -> Int -> Expr
innBuilder filterLenght dataLenght = innSum
  where
    innSum :: Int -> Expr
    innSum n = sum [x (n - j) * toExpr (vik j n) | j <- [0 .. filterLenght - 1]]
    x = xBuilder dataLenght

data ModelConfig = ModelConfig
  {
    filterLenght :: Int,
    dataLenght :: Int,
    ncpu :: Int,
    stepSize :: Double,
    sigmav :: Double
  }

-- TODO:
expandFuncBuilder :: ModelConfig -> ExpandFunc
expandFuncBuilder ModelConfig{..} = expandFunc
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

buildExpr :: ModelConfig -> Expr
buildExpr ModelConfig{..} = inn ^ 2
  where
    inn = innSum 0
    innSum = innBuilder filterLenght dataLenght

-- NOTE: Should it receive a ModelConfig too ?
iniExpandF' ::  STVar -> Double
iniExpandF' = foldl (*) 1.0 . map convert . A.toList . getTerm . getSTVar
  where
    convert (v, n) = case v of
      Var {name = 'v'} -> 1.0
      Var {name = 'u'} -> if odd n then 0 else numFactorial n * (scale ^ n)
    scale = 0.5

numFactorial :: Int -> Double
numFactorial n | n <= 1 = 1.0
               | otherwise = fromIntegral n * numFactorial ( n - 1 )

-- TODO:
numericExpandF' :: ModelConfig -> Var -> Double
numericExpandF' ModelConfig{..} v =
  case v of
    Var{name = 'a', index1 = Just i} -> 1.0 / fromIntegral (i + 1)
    Var{name = 'γ', index1 = Just n} -> numFactorial n * (scale ^ n)
    Var{name = 'σ'} -> sigmav
    Var{name = 'µ'} -> stepSize
    _ -> error $ "Unrecognized variable: " ++ show v
  where scale = 0.5

buildKernelConfig :: ModelConfig -> KernelConfig
buildKernelConfig config@ModelConfig{..} =
  KernelConfig
    { indF = isInd,
      reduceF = reduce,
      expandF = expandFuncBuilder config,
      ncpu = ncpu
    }


buildNumericalConfig :: ModelConfig -> NumericalConfig
buildNumericalConfig config =
  NumericalConfig{numericExpandF = numericExpandF' config, iniExpandF = iniExpandF'}


runModel :: ModelConfig -> KernelOutput
runModel config = kernelExpr kconfig finalExpr
  where
    finalExpr = buildExpr config
    kconfig = buildKernelConfig config
