module Main where
import Model.Classic (runModel)

import System.Environment (getArgs)

main :: IO ()
main = do
  [filterLenght, dataLenght] <- fmap (map read) getArgs
  print $ runModel filterLenght dataLenght
