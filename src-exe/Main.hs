{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Model.Classic (runModel, buildConfig, buildExpr, ModelConfig (..))
import Symbolic.Kernel (KernelOutput (..), kernelExpr, buildEvaluator)

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
    dataLenght :: Int
  } deriving (Show, Eq, Data, Typeable)

progArgs =
  ProgArgs
    { numFile = def &= help "Where to write the numerical matrices.",
      symFile = def &= help "Where to write the symbolic matrices.",
      emseFile = def &= help "Where to write the emse evolution.",
      maxIter = -1 &= help "The last iteration.",
      stepSize = -1 &= help "Step size.",
      sigmav2 = -1 &= help "variance (σᵥ²).",
      filterLenght = def &= help "Filter length." &= explicit &= name "N",
      dataLenght = def &= help "Data length." &= explicit &= name "M"
    }

isPower2 :: Int -> Bool
isPower2 n | n <= 0 = False
           | n == 1 = True
           | even n = isPower2 (n `div` 2)
           | otherwise = False

main :: IO ()
main = do
  ProgArgs{..} <- cmdArgs progArgs
  ncpu <- getNumCapabilities
  print $ "Running with " ++ show ncpu ++ " threads."

  if | filterLenght <= 0 -> putStrLn "filter lenght must be greater than 0"
     | dataLenght <= 0 -> putStrLn "data lenght must be greater than 0"
     | not . isPower2 $ ncpu -> putStrLn $ "You can only pass power of 2 number of threads!!! Current is " ++ show ncpu
     | isJust emseFile && (maxIter < 0 || stepSize < 0 || sigmav2 < 0) -> putStrLn "MaxIter, stepsize or sigmav2 is not specified."
     | otherwise -> do
        let modelConfig = ModelConfig{stepSize, sigmav = sqrt sigmav2, filterLenght, dataLenght, ncpu}
        let finalExpr = buildExpr modelConfig
        let config = buildConfig modelConfig
        let out@KernelOutput {..} = kernelExpr config finalExpr

        putStrLn $ "numberOfLevels = " ++ (show . length $ levelSize)
        putStrLn $ "levelSize = " ++ show levelSize
        putStrLn $ "numberOfEqs = " ++ (show . sum $ levelSize)

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
          let eval = buildEvaluator config out finalExpr
          let y = vectorY0 : [(matrixA !*! yk) !+! vectorB | yk <- y]
          -- let y = vectorY0 : [matrixA #> yk `add` vectorB | yk <- y]
          withFile (fromJust emseFile) WriteMode $ \h -> do
            forM_ (zip [0 .. maxIter] (map eval y)) $ \(idx, val) -> do
              hPutStrLn h $ show idx ++ " " ++ show val
