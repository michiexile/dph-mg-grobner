-- Monomial implementation for Math.GröbnerPar
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Monomial 

where

import Data.Array
import Data.Ix
import Data.Monoid
import Data.List

data Monomial = M [Int] deriving (Eq)

exponents :: Monomial -> [Int]
exponents (M ws) = ws

instance Monoid Monomial where
    mappend m n = M $ zipWith (+) (exponents m) (exponents n)
    mempty = M $ replicate 10 0

instance Show Monomial where
    show m = intercalate "*" $ buildString (exponents m) (map (:[]) ['a'..'z'] ++ map (("x" ++) . show) [(1::Int)..])

buildString [] strs = []
buildString (e:es) (s:strs) | e > 1 = (s ++ "^" ++ show e) : buildString es strs
                            | e == 1 = s : buildString es strs
                            | e < 0 = (s ++ "^" ++ show e) : buildString es strs
                            | otherwise = buildString es strs

class (Eq o) => MOrdering o where
    mcompare :: o -> Monomial -> Monomial -> Ordering
    mordering :: o

data DegRevLex = DRL deriving (Eq, Show)

instance MOrdering DegRevLex where
    mcompare o m n = if dcmp == EQ then lastLex else dcmp where
                             dcmp = compare (sum (exponents m)) (sum (exponents n))
                             lexes = zipWith compare (exponents m) (exponents n)
                             neqLexes = dropWhile (==EQ) . reverse $ lexes
                             lastLex = if null neqLexes then EQ else head neqLexes
    mordering = DRL

data (MOrdering o) => OrderedMonomial o = OM Monomial o deriving (Eq)

instance (MOrdering o) => Show (OrderedMonomial o) where
    show (OM m o) = show m

instance (MOrdering o) => Ord (OrderedMonomial o) where
    compare (OM m mo) (OM n _) = mcompare mo m n

om :: MOrdering o => Monomial -> OrderedMonomial o
om m = OM m mordering

monomial :: MOrdering o => OrderedMonomial o -> Monomial
monomial (OM m o) = m

(.^) :: (MOrdering o) => OrderedMonomial o -> Int -> OrderedMonomial o
(OM m o) .^ i = OM (mconcat (replicate (fromIntegral i) m)) o

(.*) :: (MOrdering o) => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
(OM m o) .* (OM n _) = OM (mappend m n) o

invM :: Monomial -> Monomial
invM m = M $ map negate (exponents m)

(./) :: (MOrdering o) => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
(OM m o) ./ (OM n _) = OM (mappend m (invM n)) o

totalDegree :: (MOrdering o) => OrderedMonomial o -> Int
totalDegree (OM m _) = sum (exponents m)

infixr 8 .^
infixr 7 .*
infixr 7 ./

a = OM (M [1,0,0]) DRL
b = OM (M [0,1,0]) DRL
c = OM (M [0,0,1]) DRL
one = OM (M [0,0,0]) DRL

gcdM :: Monomial -> Monomial -> Monomial
gcdM m n = M $ zipWith min (exponents m) (exponents n)

gcdOM :: MOrdering o => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
gcdOM m n = om $ gcdM (monomial m) (monomial n)

lcmM :: Monomial -> Monomial -> Monomial
lcmM m n = M $ zipWith max (exponents m) (exponents n)

lcmOM :: MOrdering o => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
lcmOM m n = om $ lcmM (monomial m) (monomial n)

