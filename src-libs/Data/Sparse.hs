-- | TODO: Write tests for this module
module Data.Sparse (module Data.Sparse) where

import Control.Monad
import Data.Vector.Generic.Mutable (new, write)
import qualified Data.Vector.Generic.Mutable as V
import Data.Vector.Unboxed (create, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed as Vb
import qualified Data.List as L

type Vector = V.Vector Double

newtype Sparse = Sparse {getSparse :: [[(Int, Double)]]} deriving (Eq, Show)

infix 7 !*!

(!*!) :: Sparse -> Vector -> Vector
(!*!) (Sparse a) b = create $ do
  let n = length a
  c <- new n
  forM_ (zip a [0 ..]) $ \(l, i) -> do
    let s = sum [aik * (b ! k) | (k, aik) <- l]
    write c i s
  return c

norm :: Vector -> Double
norm = V.sum . V.map (^2)

normalizeVec :: Vector -> Vector
normalizeVec v = V.map (/l) v
  where
    l = norm v

infixl 6 !+!

(!+!) :: Vector -> Vector -> Vector
(!+!) = V.zipWith (+)

infix 7 !.!

a !.! b = V.sum $ V.zipWith (*) a b


powerMethodIteration :: Sparse -> [Double]
powerMethodIteration a = eigen
  where
    n = length . getSparse $ a
    b0 = Vb.generate n (const 1)
    b = b0 : [ normalizeVec bk | bkl1 <- b, let bk = a !*! bkl1]
    eigen = [(bk !.! (a !*! bk)) / (bk !.! bk) | bk <- tail b]

maxEigenValue :: Sparse -> Maybe Double
maxEigenValue a = fmap snd . L.find (\(f,s) -> abs(s - f) <= abs f * precision) . take maxIterNumber $ zip eigenList (tail eigenList)
  where
    eigenList = powerMethodIteration a
    precision = 10 ** (-5)
    maxIterNumber = 10^4
