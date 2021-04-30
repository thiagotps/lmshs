{-# LANGUAGE RecordWildCards #-}
module Main where
import Model.Classic (runModel)
import Symbolic.Kernel (KernelOutput (..))

import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  [filterLenght, dataLenght] <- fmap (map read) getArgs
  let KernelOutput{..} = runModel filterLenght dataLenght
  putStrLn $ "numberOfLevels = " ++ (show . length $ levelSize)
  putStrLn $ "levelSize = " ++ show levelSize
  putStrLn $ "numberOfEqs = " ++ (show . sum $ levelSize)
  withFile "num.txt" WriteMode $ \h -> do
    hPrint h matrixA
    hPrint h vectorB
  withFile "sym.txt" WriteMode $ \h -> do
    hPrint h stateVarList
