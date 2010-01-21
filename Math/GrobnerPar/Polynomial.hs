-- Polynomial implementation for the GröbnerPar project
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Polynomial 

where

import Data.Map (Map, (!))
import qualified Data.Map as DM
import Math.GrobnerPar.Monomial
import Data.List (intercalate)
import Data.Monoid (mappend, mempty)
import Data.Maybe (fromMaybe)

data (Num r, MOrdering o) => Polynomial r o = P {
      getMap :: Map (OrderedMonomial o) r
    } deriving (Eq, Ord)

instance (Num r, MOrdering o) => Show (Polynomial r o) where
    show (P m) = if null s then "0" else s where
        s = intercalate " + " . reverse . fst $ 
            DM.mapAccumWithKey foo [] m 
                where
                  foo acc key value = (addTerm acc key value, value)
                  addTerm acc key value  | value == fromInteger 0 = acc
                                         | totalDegree key == 0 = acc ++ [show value]
                                         | value == fromInteger 1 = acc ++ [show key]
                                         | otherwise = acc ++ [show value ++ "*" ++ show key]

(*.) :: (Num r, MOrdering o) => OrderedMonomial o -> Polynomial r o -> Polynomial r o
m *. p = P $ DM.mapKeys (.* m) (getMap p)

infixr 5 *.

instance (Num r, MOrdering o) => Num (Polynomial r o) where
--  (+) :: Polynomial r o -> Polynomial r o -> Polynomial r o
  p + q = P $ DM.unionWith (+) (getMap p) (getMap q)
--  (*) :: Polynomial r o -> Polynomial r o -> Polynomial r o
  p * q = P $ DM.fromListWith (+) $ do
            (pm,pc) <- pL
            (qm,qc) <- qL
            return (pm .* qm, pc * qc)
                where
                  pL = DM.toList . getMap $ p
                  qL = DM.toList . getMap $ q
--  (-) :: Polynomial r o -> Polynomial r o -> Polynomial r o
  p - q = p + (negate q)
--  negate :: Polynomial r o -> Polynomial r o
  negate p = P $ DM.map negate (getMap p)
--  abs :: Polynomial r o -> Polynomial r o
  abs _ = undefined
--  signum :: Polynomial r o -> Polynomial r o
  signum _ = undefined
--  fromInteger :: Integer -> Polynomial r o
  fromInteger n = P $ DM.fromList [(om mempty, fromInteger n)]

leadingTerm :: (Num r, MOrdering o) => Polynomial r o -> (OrderedMonomial o, r)
leadingTerm p = fst $ fromMaybe ((om mempty, 0), getMap p) (DM.maxViewWithKey (getMap p))
