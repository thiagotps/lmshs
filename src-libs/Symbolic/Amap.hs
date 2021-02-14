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
import Data.Hashable (Hashable)
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Internal.Strict as M

-- NOTE: Amap stands for Algebra Map.
newtype Amap a b = Amap {getAMap :: HashMap a b} deriving (Eq,Hashable)

instance (Num b, Hashable a, Eq b, Eq a) => (Semigroup (Amap a b)) where
  (<>) (Amap m) (Amap n) = Amap . filterNonZero . M.unionWith (+) m $ n

instance (Num b, Hashable a, Eq b, Eq a) => Monoid (Amap a b) where
  mempty = Amap mempty

filterNonZero :: (Num b, Eq b) => HashMap a b -> HashMap a b
filterNonZero  = M.filter (/= 0)

fromList :: (Hashable a, Num b, Eq b, Eq a) => [(a, b)] -> Amap a b
fromList = Amap . filterNonZero . M.fromListWith (+)

toList :: Amap a b -> [(a, b)]
toList = M.toList . getAMap

toListKeys :: Amap a b -> [a]
toListKeys = Prelude.map fst . toList

mapKeysWith ::   (Hashable k2, Eq k2) => (a -> a -> a) -> (k1 -> k2) -> HashMap k1 a -> HashMap k2 a
mapKeysWith g f = M.fromListWith g . Prelude.map (\(x,y) -> (f x, y)) . M.toList

mapKey :: (Hashable c, Num b, Eq b, Eq c) => (a -> c) -> Amap a b -> Amap c b
mapKey f = Amap . filterNonZero . mapKeysWith (+) f . getAMap

map :: (Eq c, Num c, Num b) => (b -> c) -> Amap a b -> Amap a c
map f  = Amap . filterNonZero . M.map f . getAMap

singleton :: (Hashable a, Num b, Eq b) => a -> b -> Amap a b
singleton a b = Amap . filterNonZero $ M.singleton a b
