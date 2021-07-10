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
    startLevelForIA :: Maybe Int,
    maxStepSize :: Bool,
    binarySearchPrecision :: Int
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
      startLevelForIA = def &= help "The level to start applying the IA assumption.",
      maxStepSize = def &=  help "Compute the maximum step-size",
      binarySearchPrecision = def &= opt (-4 :: Int) &= typ "INT"
      &= help "The expoent describing the precision (i.e: if this is n than the precision is 10**(-n))"
    }

isPower2 :: Int -> Bool
isPower2 n | n <= 0 = False
           | n == 1 = True
           | even n = isPower2 (n `div` 2)
           | otherwise = False

isMatrixGood :: Sparse -> Maybe Bool
isMatrixGood m = (\b -> abs b < 1.0) <$> maxEigenValue m

buildCustomMatrix :: (ModelConfig, SparseSymbolic) -> Double -> Sparse
buildCustomMatrix (mc,sparseSym) stepSize = matrixA
  where
    nc = buildNumericalConfig mc{Model.Classic.stepSize}
    NumericalMatrices{matrixA,..} = buildNumMatrices nc sparseSym

binarySearch :: Double -> (ModelConfig,SparseSymbolic) -> Double -> Double -> Either Sparse Double
binarySearch precision c =  bs
  where
    check mid = let m = buildCustomMatrix c mid in
                  case isMatrixGood m of
                    Just r -> Right r
                    Nothing -> Left m

    bs :: Double -> Double -> Either Sparse Double
    bs low high | abs (low - high) < precision = Right mid
                | otherwise = do
                    ck <- check mid
                    if ck
                      then bs mid high
                      else bs low mid

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
        let modelConfig = ModelConfig{stepSize, sigmav = sqrt sigmav2, filterLenght, dataLenght, ncpu, startLevelForIA}
        let finalExpr = buildExpr modelConfig
        let kernelConfig = buildKernelConfig modelConfig
        let numericalConfig = buildNumericalConfig modelConfig
        let out@KernelOutput {..} = kernelExpr kernelConfig finalExpr

        case startLevelForIA of
          Just s -> putStrLn $ "startLevelForIA = " ++ show s
          Nothing -> return ()

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
          let precision = 10 ** fromIntegral binarySearchPrecision
          case binarySearch precision (modelConfig, stateVars) 0.01 1 of
            Right m -> putStrLn $ "Maximum step-size = " ++ show m
            Left s -> do
              let logFile = "lmshs.log"
              putStrLn $ "Failed to compute maximum step-size. Writing details to " ++ logFile ++ " ..."
              withFile logFile WriteMode $ \h -> hPrint h s
