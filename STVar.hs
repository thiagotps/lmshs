-- |

module STVar  where
import Expr
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L


data STSum = STSum (Map STVar Expr) Expr
newtype STVar = STVar Term deriving (Ord, Eq)

instance Show STVar where
  show (STVar t) = "E[" ++ show t ++ "]"

instance Show STSum where
  show (STSum m a) = L.intercalate " + " $ [show cnt ++ "*" ++ show mul | (mul, cnt) <- M.toList m] ++ [show a]

instance Semigroup STSum where
  (<>) (STSum m a) (STSum n b) = STSum p (a+b)
    where p = M.filter (/=0) (M.unionWith (+) m n)

instance Monoid STSum where
  mempty = STSum mempty 0
