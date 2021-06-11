-- |
module Symbolic.Sparse (module Symbolic.Sparse) where

import Control.Monad
import Data.Vector.Generic.Mutable (new, write)
import qualified Data.Vector.Generic.Mutable as V
import Data.Vector.Unboxed (create, (!))
import qualified Data.Vector.Unboxed as V

type Vector = V.Vector Double

newtype Sparse = Sparse [[(Int, Double)]] deriving (Eq, Show)

infix 7 !*!

(!*!) :: Sparse -> Vector -> Vector
(!*!) (Sparse a) b = create $ do
  let n = length a
  c <- new n
  forM_ (zip a [0 ..]) $ \(l, i) -> do
    let s = sum [aik * (b ! k) | (k, aik) <- l]
    write c i s
  return c

infixl 6 !+!

(!+!) :: Vector -> Vector -> Vector
(!+!) = V.zipWith (+)

infix 7 !.!

a !.! b = V.sum $ V.zipWith (*) a b
