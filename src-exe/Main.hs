{-# LANGUAGE RecordWildCards #-}
module Main where
import Model.Classic (runModel, buildConfig, buildExpr)
import Symbolic.Kernel (KernelOutput (..), kernelExpr, buildEvaluator)

import System.Environment (getArgs)
import System.IO
import qualified Data.Massiv.Array as D
import qualified Data.Massiv.Array.Mutable as D
import Control.Monad
import Symbolic.Sparse
import Control.Concurrent (getNumCapabilities)


isPower2 :: Int -> Bool
isPower2 n | n <= 0 = False
           | n == 1 = True
           | even n = isPower2 (n `div` 2)
           | otherwise = False

main :: IO ()
main = do
  [filterLenght, dataLenght] <- fmap (map read) getArgs
  ncpu <- getNumCapabilities
  print $ "Running with " ++ show ncpu ++ " threads."
  if not . isPower2 $ ncpu
    then print "You can only pass power of 2 number of threads!!!"
    else do
      let finalExpr = buildExpr filterLenght dataLenght
      let config = buildConfig filterLenght dataLenght ncpu
      let out@KernelOutput {..} = kernelExpr config finalExpr

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
      let y = vectorY0 : [(matrixA !*! yk) !+! vectorB | yk <- y]
      -- let y = vectorY0 : [matrixA #> yk `add` vectorB | yk <- y]
      withFile "emse.txt" WriteMode $ \h -> do
        forM_ (zip [0 .. 1000] (map eval y)) $ \(idx, val) -> do
          hPutStrLn h $ show idx ++ " " ++ show val
