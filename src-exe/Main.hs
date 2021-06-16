{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Model.Classic (runModel, buildKernelConfig, buildExpr, ModelConfig (..), buildNumericalConfig)
import Symbolic.Kernel (KernelOutput (..), kernelExpr, buildEvaluator, NumericalConfig (..), buildNumMatrices, NumericalMatrices (NumericalMatrices, matrixA, vectorY0, vectorB), SparseSymbolic)

import System.Environment (getArgs)
import System.IO
import Control.Monad
import Data.Sparse
import Control.Concurrent (getNumCapabilities)
import Data.Maybe

import System.Console.CmdArgs
import Control.Applicative


data ProgArgs = ProgArgs
  { numFile :: Maybe String,
    symFile :: Maybe String,
    emseFile :: Maybe String,
    maxIter :: Int,
    stepSize :: Double,
    sigmav2 :: Double,
    filterLenght :: Int,
    dataLenght :: Int,
    maxStepSize :: Bool
  } deriving (Show, Eq, Data, Typeable)

progArgs =
  ProgArgs
    { numFile = def &= help "Where to write the numerical matrices.",
      symFile = def &= help "Where to write the symbolic matrices.",
      emseFile = def &= help "Where to write the emse evolution.",
      maxIter = -1 &= help "The last iteration.",
      stepSize = -1 &= help "step-size.",
      sigmav2 = -1 &= help "variance (σᵥ²).",
      filterLenght = def &= help "Filter length." &= explicit &= name "N",
      dataLenght = def &= help "Data length." &= explicit &= name "M",
      maxStepSize = def &=  help "Compute the maximum step-size"
    }

isPower2 :: Int -> Bool
isPower2 n | n <= 0 = False
           | n == 1 = True
           | even n = isPower2 (n `div` 2)
           | otherwise = False

isMatrixGood :: Sparse -> Bool
isMatrixGood m = case maxEigenValue m of
  Just b -> abs b < 1.0
  Nothing -> error "It was not possible to find the maximum eigenvalue for this matrix !"

buildCustomMatrix :: (ModelConfig, SparseSymbolic) -> Double -> Sparse
buildCustomMatrix (mc,sparseSym) stepSize = matrixA
  where
    nc = buildNumericalConfig mc{Model.Classic.stepSize}
    NumericalMatrices{matrixA,..} = buildNumMatrices nc sparseSym

binarySearch :: (ModelConfig,SparseSymbolic) -> Double -> Double -> Double
binarySearch c =  bs
  where
    precision = 10 ** (-4)
    check = isMatrixGood . buildCustomMatrix c

    bs :: Double -> Double -> Double
    bs low high | abs (low - high) < precision = mid
                | otherwise = if check mid then bs mid high else bs low mid
      where
        mid = (low + high) / 2


main :: IO ()
main = do
  ProgArgs{..} <- cmdArgs progArgs
  ncpu <- getNumCapabilities
  print $ "Running with " ++ show ncpu ++ " threads."

  if | filterLenght <= 0 -> putStrLn "filter lenght must be greater than 0"
     | dataLenght <= 0 -> putStrLn "data lenght must be greater than 0"
     | not . isPower2 $ ncpu -> putStrLn $ "You can only pass power of 2 number of threads!!! Current is " ++ show ncpu
     | isJust emseFile && (maxIter < 0 || stepSize < 0 || sigmav2 < 0) -> putStrLn "MaxIter, stepsize or sigmav2 is not specified."
     | maxStepSize && (sigmav2 < 0) -> putStrLn "sigmav2 was not specified"
     | otherwise -> do
        let modelConfig = ModelConfig{stepSize, sigmav = sqrt sigmav2, filterLenght, dataLenght, ncpu}
        let finalExpr = buildExpr modelConfig
        let kernelConfig = buildKernelConfig modelConfig
        let numericalConfig = buildNumericalConfig modelConfig
        let out@KernelOutput {..} = kernelExpr kernelConfig finalExpr

        putStrLn $ "numberOfLevels = " ++ (show . length $ levelSize)
        putStrLn $ "levelSize = " ++ show levelSize
        putStrLn $ "numberOfEqs = " ++ (show . sum $ levelSize)

        let NumericalMatrices{matrixA, vectorY0, vectorB} = buildNumMatrices numericalConfig stateVars

        when (isJust numFile) $
          withFile (fromJust numFile) WriteMode $ \h -> do
            hPutStrLn h "Matrix A: "
            hPrint h matrixA
            hPutStrLn h "Vector Y: "
            hPrint h vectorY0
            hPutStrLn h "Vector B: "
            hPrint h vectorB

        when (isJust symFile) $
          withFile (fromJust symFile) WriteMode $ \h -> do
            hPrint h stateVarList

        when (isJust emseFile) $ do
          let eval = buildEvaluator numericalConfig finalExpr
          let y = vectorY0 : [(matrixA !*! yk) !+! vectorB | yk <- y]
          -- let y = vectorY0 : [matrixA #> yk `add` vectorB | yk <- y]
          withFile (fromJust emseFile) WriteMode $ \h -> do
            forM_ (zip [0 .. maxIter] (map eval y)) $ \(idx, val) -> do
              hPutStrLn h $ show idx ++ " " ++ show val

        when maxStepSize $ do
          putStrLn "Computing the maximum step-size ..."
          let m = binarySearch (modelConfig, stateVars) 0.01 1
          putStrLn $ "Maximum step-size = " ++ show m
