-- |
module Symbolic.Amap
  ( Amap,
    getAMap,
    fromList,
    toList,
    toListKeys,
    mapKey,
    Symbolic.Amap.map,
    singleton
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- NOTE: Amap stands for Algebra Map.
newtype Amap a b = Amap {getAMap :: Map a b} deriving (Eq, Ord, Show)

instance (Num b, Ord a, Eq b) => (Semigroup (Amap a b)) where
  (<>) (Amap m) (Amap n) = Amap . filterNonZero . M.unionWith (+) m $ n

instance (Num b, Ord a, Eq b) => Monoid (Amap a b) where
  mempty = Amap mempty

filterNonZero :: (Num b, Eq b) => Map a b -> Map a b
filterNonZero  = M.filter (/= 0)

fromList :: (Ord a, Num b, Eq b) => [(a, b)] -> Amap a b
fromList = Amap . filterNonZero . M.fromListWith (+)

toList :: Amap a b -> [(a, b)]
toList = M.toList . getAMap

toListKeys :: Amap a b -> [a]
toListKeys = Prelude.map fst . toList

mapKey :: (Ord c, Num b, Eq b) => (a -> c) -> Amap a b -> Amap c b
mapKey f = Amap . filterNonZero . M.mapKeysWith (+) f . getAMap

map :: (Eq c, Num c, Num b) => (b -> c) -> Amap a b -> Amap a c
map f  = Amap . filterNonZero . M.map f . getAMap

singleton :: (Ord a, Num b, Eq b) => a -> b -> Amap a b
singleton a b = Amap . filterNonZero $ M.singleton a b
