module Main where
import Expr

main :: IO ()
main = let x = fromVar . mkVar $ 'x'
           y = fromVar . mkVar $ 'y'
           in print ((x+y)^100)
