{-| Monomial implementation.
 -}

-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Monomial 

where

import Data.Array
import Data.Ix
import Data.Monoid
import Data.List
import Data.Word

-- | Fundamental monomial type. A monomial is its sequence of exponents.
data Monomial = M [Int] deriving (Eq)

-- | Retrieve the sequence of exponents from a monomial.
exponents :: Monomial -> [Int]
exponents (M ws) = ws

-- | Monoid instance for Monomial. Note that the mempty implementation
-- randomly assumes some arbitrary number of variables in the
-- polynomial ring. This is probably bad.
instance Monoid Monomial where
    mappend m n = M $ zipWith (+) (exponents m) (exponents n)
    mempty = M $ replicate 10 0

-- | Show instance for Monomial. This currently mandates the variable
-- names a, b, c, ..., z, x1, x2, x3, x4, ..., x47, x48, ..., x1729,
-- x1730, ...
-- This too is probably bad.
instance Show Monomial where
    show m = intercalate "*" $ buildString (exponents m) (map (:[]) ['a'..'z'] ++ map (("x" ++) . show) [(1::Int)..])

buildString [] strs = []
buildString (e:es) (s:strs) | e > 1 = (s ++ "^" ++ show e) : buildString es strs
                            | e == 1 = s : buildString es strs
                            | e < 0 = (s ++ "^" ++ show e) : buildString es strs
                            | otherwise = buildString es strs

-- | Fundamental typeclass for monomial orderings.
class (Eq o) => MOrdering o where
    -- | Monomial ordering compare. This works like Prelude.compare,
    -- but comes with a parameter belonging to the MOrdering class. 
    -- We require that mcompare m n = mcompare (a.*m) (a.*n) for all
    -- monomials a, m, n.
    mcompare :: o -> Monomial -> Monomial -> Ordering
    -- | "Magic" retrieval of the ordering type itself. This makes it
    -- possible to work with many higher level constructors using the
    -- type system instead of retyping the ordering constructor over
    -- and over.
    mordering :: o

-- | Degree reverse lexicographic order. m < n if either deg m < deg n
-- or degrees equal the last non-equal exponent is smaller in m.
data DegRevLex = DRL deriving (Eq, Show)
instance MOrdering DegRevLex where
    mcompare o m n = if dcmp == EQ then lastLex else dcmp where
                             dcmp = compare (totalDegreeM m) (totalDegreeM n)
                             lexes = zipWith compare (exponents m) (exponents n)
                             neqLexes = dropWhile (==EQ) . reverse $ lexes
                             lastLex = if null neqLexes then EQ else head neqLexes
    mordering = DRL


-- | Degree lexicographic order. m < n if either deg m < deg n or
-- degrees equal the first non-equal exponent is smaller in m.
data DegLex = DL deriving (Eq, Show)
instance MOrdering DegLex where
    mcompare o m n = if dcmp == EQ then firstLex else dcmp where
                             dcmp = compare (totalDegreeM m) (totalDegreeM n)
                             lexes = zipWith compare (exponents m) (exponents n)
                             neqLexes = dropWhile (==EQ) lexes
                             firstLex = if null neqLexes then EQ else head neqLexes
    mordering = DL


-- | Lexicographic order.
data Lex = L deriving (Eq, Show)
instance MOrdering Lex where
    mcompare o m n = compare (exponents m) (exponents n)
    mordering = L





-- | Ordered monomials are monomials with an attached ordering.
data (MOrdering o) => OrderedMonomial o = OM Monomial o deriving (Eq)

-- | Show instance hides the order for human legibility. Do we maybe
-- want to use a prettyprinter interface instead and derive all show instances?
instance (MOrdering o) => Show (OrderedMonomial o) where
    show (OM m o) = show m

-- | Ord instance derived from the monomial ordering attached.
instance (MOrdering o) => Ord (OrderedMonomial o) where
    compare (OM m mo) (OM n _) = mcompare mo m n

-- | Smart-ish constructor for ordered monomials.
om :: MOrdering o => Monomial -> OrderedMonomial o
om m = OM m mordering

-- | Destructor returning the monomial for a given ordered monomial.
monomial :: MOrdering o => OrderedMonomial o -> Monomial
monomial (OM m o) = m

-- | Integral exponentiation for ordered monomials.
(.^) :: (MOrdering o) => OrderedMonomial o -> Word -> OrderedMonomial o
(OM m o) .^ i = OM (mconcat (replicate (fromIntegral i) m)) o

-- | Multiplication of ordered monomials.
(.*) :: (MOrdering o) => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
(OM m o) .* (OM n _) = OM (mappend m n) o

-- | Inversion of a monomial: negation of all the exponents.
invM :: Monomial -> Monomial
invM m = M $ map negate (exponents m)

-- | Division of monomials.
(./) :: (MOrdering o) => OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
(OM m o) ./ (OM n _) = OM (mappend m (invM n)) o

-- | Total degree of a monomial.
totalDegreeM :: Monomial -> Int
totalDegreeM = sum . exponents

-- | Total degree of an ordered monomial.
totalDegree :: (MOrdering o) => OrderedMonomial o -> Int
totalDegree (OM m _) = totalDegreeM m

infixr 8 .^
infixr 7 .*
infixr 7 ./

a = OM (M [1,0,0]) DRL
b = OM (M [0,1,0]) DRL
c = OM (M [0,0,1]) DRL
one = OM (M [0,0,0]) DRL

-- | GCD for monomials by taking min of each exponent.
gcdM :: Monomial -> Monomial -> Monomial
gcdM m n = M $ zipWith min (exponents m) (exponents n)

-- | GCD for ordered monomials, handling the wrapping and unwrapping.
gcdOM :: MOrdering o => 
         OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
gcdOM m n = om $ gcdM (monomial m) (monomial n)

-- | LCM for monomials by taking max of each exponent.
lcmM :: Monomial -> Monomial -> Monomial
lcmM m n = M $ zipWith max (exponents m) (exponents n)

-- | LCM for ordered monomials, handling the wrapping and unwrapping.
lcmOM :: MOrdering o => 
         OrderedMonomial o -> OrderedMonomial o -> OrderedMonomial o
lcmOM m n = om $ lcmM (monomial m) (monomial n)

-- | Change the ordering of an ordered monomial to a different
-- monomial order.
changeOrderingM :: (MOrdering o, MOrdering p) => 
                   OrderedMonomial o -> OrderedMonomial p
changeOrderingM m = om $ monomial m

-- | Tests whether a given monomial divides another.
divides :: (MOrdering o) => 
           OrderedMonomial o -> OrderedMonomial o -> Bool
divides n m = all id $ zipWith (<=) 
              (exponents . monomial $ n) 
              (exponents . monomial $ m)

