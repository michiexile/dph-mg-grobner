-- Monomial implementation for Math.GröbnerPar
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Monomial 

where

import Data.Array
import Data.Ix
import Data.Word
import Data.Monoid
import Data.List

data Monomial = M {
      exponents :: [Word]
    } deriving (Eq)

instance Monoid Monomial where
    mappend m n = M $ zipWith (+) (exponents m) (exponents n)
    mempty = M $ repeat 0

(.^) :: Monomial -> Word -> Monomial
m .^ i = mconcat (replicate (fromIntegral i) m)

(.*) :: Monomial -> Monomial -> Monomial
m .* n = mappend m n

infixr 8 .^
infixr 7 .*

instance Show Monomial where
    show m = intercalate "*" $ buildString (exponents m) (map (:[]) ['a'..'z'] ++ map (("x" ++) . show) [(1::Word)..])

buildString [] strs = []
buildString (e:es) (s:strs) = if e > 0 then (s ++ "^" ++ show e) : buildString es strs else buildString es strs

a = M [1,0,0]
b = M [0,1,0]
c = M [0,0,1]

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


