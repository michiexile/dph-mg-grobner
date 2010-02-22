-- | Implementation of the Buchberger algorithm for the GrobnerPar project.
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Buchberger 
where

import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial
import Data.List (nub)
import Debug.Trace

-- | Find the S-polynomial of f and g, defined as
-- lcm(lt f, lt g)/lt f * f - lcm(lt f, lt g)/lt g * g
findSpolynomial :: (Fractional r, MOrdering o) =>
                   Polynomial r o -> Polynomial r o -> Polynomial r o
findSpolynomial f g | isZero f = 0
                    | isZero g = 0
                    | totalDegree (gcdOM lmf lmg) == 0 = 0
                    | otherwise = (lcmf *. f) - (lcmg *. ((constantTerm c) * g))
                      where
                        (lmf, lcf) = leadingTerm f
                        (lmg, lcg) = leadingTerm g
                        c = lcf/lcg
                        lcm = lcmOM lmf lmg
                        lcmf = lcm ./ lmf
                        lcmg = lcm ./ lmg

-- | Find all S-polynomials of all pairs of the polynomials in the basis given in ps.
findAllSpolynomials :: (Fractional r, MOrdering o) => 
                       [Polynomial r o] -> [Polynomial r o] -> [Polynomial r o]
findAllSpolynomials olds news = nub . filter (not . isZero) $ do 
                                  p <- olds ++ news
                                  q <- news
                                  return $ findSpolynomial p q

-- | Return p - lt p / lt q * q unless q=0, in which case it returns p unchanged.
reducePolynomial :: (Fractional r, MOrdering o) =>
                    Polynomial r o -> Polynomial r o -> Polynomial r o
reducePolynomial p q | isZero p = 0
                     | isZero q = p
                     | otherwise = p - (pq *. (constantTerm c) * q)
                     where
                       (lmp, lcp) = leadingTerm p
                       (lmq, lcq) = leadingTerm q
                       pq = lmp ./ lmq
                       c = lcp / lcq

-- | Reduce a polynomial wrt all polynomials in a set.
reduceFull :: (Fractional r, MOrdering o) =>
              Polynomial r o -> [Polynomial r o] -> Polynomial r o
reduceFull p qs = if isZero p then p else
                  if null reducables 
                  then p
                  else reduceFull (reducePolynomial p (head reducables)) qs
                      where
                        (headP, _) = leadingTerm p
                        reducables = dropWhile (not . (`divides` headP) . fst . leadingTerm) qs

-- | Reduce all elements of ps completely vs. the generators in qs.
reduceAllFull :: (Fractional r, MOrdering o) => 
                 [Polynomial r o] -> [Polynomial r o] -> [Polynomial r o]
reduceAllFull ps qs = nub . filter (not . isZero) $ map (flip reduceFull qs) ps

-- | Stepping the Buchberger algorithm.
grobnerBasisStep old [] = old
grobnerBasisStep old new = grobnerBasisStep old' new'
                           where
                             newSps = findAllSpolynomials old new
                             newolds = reduceAllFull new old
                             new' = reduceAllFull newSps (old ++ new)
                             old' = old ++ newolds


-- | Generate S-polynomials and reduce them until no new non-reduceable S-polynomials appear.
grobnerBasis :: (Fractional r, MOrdering o) =>
                [Polynomial r o] -> [Polynomial r o]
grobnerBasis gens = grobnerBasisStep [] gens
