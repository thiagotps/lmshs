{-# LANGUAGE RecordWildCards #-}
module Main where
import Model.Classic (runModel, buildConfig, buildExpr)
import Symbolic.Kernel (KernelOutput (..), buildEvaluator, kernelExpr)

import System.Environment (getArgs)
import System.IO
import Numeric.LinearAlgebra ((#>))
import Numeric.LinearAlgebra.HMatrix (add)
import Control.Monad

main :: IO ()
main = do
  [filterLenght, dataLenght] <- fmap (map read) getArgs
  let finalExpr = buildExpr filterLenght dataLenght
  let config = buildConfig filterLenght dataLenght
  let out@KernelOutput{..} = kernelExpr config finalExpr
  putStrLn $ "numberOfLevels = " ++ (show . length $ levelSize)
  putStrLn $ "levelSize = " ++ show levelSize
  putStrLn $ "numberOfEqs = " ++ (show . sum $ levelSize)
  withFile "num.txt" WriteMode $ \h -> do
    hPutStrLn h "Matrix A: "
    hPrint h matrixA
    hPutStrLn h "Vector Y: "
    hPrint h vectorY0
    hPutStrLn h "Vector B: "
    hPrint h vectorB
  withFile "sym.txt" WriteMode $ \h -> do
    hPrint h stateVarList

  let eval = buildEvaluator config out finalExpr
  let y = vectorY0 : [matrixA #> yk `add` vectorB | yk <- y]
  withFile "emse.txt" WriteMode $ \h -> do
    forM_ (zip [0..1000] (map eval y)) $ \(idx, val) -> do
      hPutStrLn h $ show idx ++ " " ++ show val
