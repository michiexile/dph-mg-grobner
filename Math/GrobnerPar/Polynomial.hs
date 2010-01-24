{-| Polynomial implementation.
 -}

-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Polynomial where

import Data.Map (Map, (!))
import qualified Data.Map as DM
import Data.List (intercalate)
import Data.Monoid (mappend, mempty)
import Data.Maybe (fromMaybe)

import Math.GrobnerPar.Monomial




{-| Fundamental type of a polynomial. It is carried, under the hood, by a Map,
    giving us nice complexity guarantees on finding the leading term and on
    various operations.
 -}
data (Num r, MOrdering o) => Polynomial r o = P {
      getMap :: Map (OrderedMonomial o) r
    } deriving (Eq, Ord)

-- | Pretty printing of polynomials.
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

{-| Multiplying a polynomial by a single monomial. Will be used a lot in the
    Buchberger algorithm. The properties of MOrdering allows us to use
    mapKeysMonotonic.
 -}
(*.) :: (Num r, MOrdering o) => OrderedMonomial o -> Polynomial r o -> Polynomial r o
m *. p = P $ DM.mapKeysMonotonic (.* m) (getMap p)

infixr 5 *.

removeNulls :: (Num r, MOrdering o) => 
               Polynomial r o -> Polynomial r o
removeNulls (P p) = P $ DM.filter (/= 0) p

instance (Num r, MOrdering o) => Num (Polynomial r o) where
--  (+) :: Polynomial r o -> Polynomial r o -> Polynomial r o
  p + q = removeNulls $ P $ DM.unionWith (+) (getMap p) (getMap q)
--  (*) :: Polynomial r o -> Polynomial r o -> Polynomial r o
  p * q = removeNulls $ P $ DM.fromListWith (+) $ do
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

changeOrdering :: (Num r, MOrdering o, MOrdering p) => 
                  Polynomial r o -> Polynomial r p
changeOrdering p = P $ DM.mapKeys changeOrderingM (getMap p)

constantTerm :: (Num r, MOrdering o) => r -> Polynomial r o
constantTerm x = removeNulls $ P $ DM.fromList [(om mempty, x)]

monomialTerm :: (Num r, MOrdering o) => OrderedMonomial o -> Polynomial r o
monomialTerm m = P $ DM.fromList [(m, 1)]

isZero :: (Num r, MOrdering o) => Polynomial r o -> Bool
isZero = (0 ==) . snd . leadingTerm 
