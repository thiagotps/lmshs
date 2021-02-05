module Main where
import Expr

main :: IO ()
main = let x = exprFromVar . mkVar $ 'α'
           y = exprFromVar . mkVar $ 'β'
           in print ((x+y)^20)
