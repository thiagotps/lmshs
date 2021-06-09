{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Model.Classic (runModel, buildConfig, buildExpr)
import Symbolic.Kernel (KernelOutput (..), kernelExpr, buildEvaluator)

import System.Environment (getArgs)
import System.IO
import Control.Monad
import Symbolic.Sparse
import Control.Concurrent (getNumCapabilities)
import Data.Maybe

import System.Console.CmdArgs
import Control.Applicative

data ProgArgs = ProgArgs
  { numFile :: Maybe String,
    symFile :: Maybe String,
    emseFile :: Maybe String,
    filterLenght :: Int,
    dataLenght :: Int
  } deriving (Show, Eq, Data, Typeable)

progArgs =
  ProgArgs
    { numFile = def &= help "Where to write the numerical matrices.",
      symFile = def &= help "Where to write the symbolic matrices.",
      emseFile = def &= help "Where to write the emse evolution.",
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

  when (filterLenght <= 0) $ do
    putStrLn "filter lenght must be greater than 0"
    empty

  when (dataLenght <= 0) $ do
    putStrLn "data lenght must be greater than 0"
    empty

  unless (isPower2 ncpu) $ do
    putStrLn "You can only pass power of 2 number of threads!!!"
    empty


  let finalExpr = buildExpr filterLenght dataLenght
  let config = buildConfig filterLenght dataLenght ncpu
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
      forM_ (zip [0 .. 1000] (map eval y)) $ \(idx, val) -> do
        hPutStrLn h $ show idx ++ " " ++ show val
