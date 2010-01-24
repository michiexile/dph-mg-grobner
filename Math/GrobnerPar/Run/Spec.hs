{-| Job specification -- types. 
 -}
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson
module Math.GrobnerPar.Run.Spec where

import Data.Word

import qualified Data.Numbers.Primes

import Math.GrobnerPar.Monomial




data Spec                    =  Spec [[Monomial]] Field Order
deriving instance Show Spec


data Field                   =  Finite Prime
                             |  Rationals 
                             |  Real Precision
                             |  Complex Precision Label
deriving instance Show Field

newtype Prime                =  Prime Integer
deriving instance Eq Prime
deriving instance Ord Prime
deriving instance Show Prime
instance Num Prime where
  fromInteger n | n == p     =  Prime n
                | otherwise  =  error (show n ++ " is not prime.")
   where
    p:_                      =  dropWhile (< n) Data.Numbers.Primes.primes

data Precision               =  Single | Digits Word Word
deriving instance Show Precision


data Order                   =  Order [OrderSpec]
deriving instance Show Order

data OrderSpec               =  Lexicographic
                             |  ReverseLexicographic
                             |  DegreeReverseLexicographic
                             |  DegreeLexicographic
                             |  WeightedReverseLexicographic [Natural]
                             |  WeightedLexicographic [Natural]
                             |  NegativeLexicographic
                             |  NegativeDegreeReverseLexicographic
                             |  NegativeDegreeLexicographic
                             |  GeneralWeightedReverseLexicographic [Integer]
                             |  GeneralWeightedLexicographic [Integer]
                             |  Matrix [[Integer]]
deriving instance Show OrderSpec

newtype Natural              =  Natural Integer
deriving instance Eq Natural
deriving instance Ord Natural
deriving instance Show Natural
instance Num Natural where
  Natural x + Natural y      =  Natural (x + y)
  Natural x * Natural y      =  Natural (x * y)
  fromInteger n | n >= 0     =  Natural n
                | otherwise  =  error "Naturals are non-negative"



