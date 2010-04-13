{-# LANGUAGE PArr, ParallelListComp #-}
-- | Implementation of the Buchberger algorithm for the GrobnerPar project.
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Buchberger 
where

import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial
import Data.List (nub, minimumBy)
import qualified Data.Map as M
import Control.Monad (guard)

import qualified Data.Array.Parallel.Prelude as DPH
import GHC.PArr
import GHC.Conc (pseq)

import Debug.Trace


-- | Find the S-polynomial of f and g, defined as
-- lcm(lt f, lt g)/lt f * f - lcm(lt f, lt g)/lt g * g, returning 0 unless
-- psi f g = True
findSpolynomialConditional :: (Fractional r, MOrdering o) =>
                              (Polynomial r o -> Polynomial r o -> Bool) -> 
                              Polynomial r o -> 
                              Polynomial r o -> 
                              Polynomial r o
findSpolynomialConditional psi f g | not (psi f g) = 0
                                   | isZero f = 0
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

findSpolynomial :: (Fractional r, MOrdering o) =>
                              Polynomial r o -> Polynomial r o -> Polynomial r o
findSpolynomial = findSpolynomialConditional (const (const True))

-- | Find all S-polynomials of all pairs of the polynomials in the basis given in ps such that
-- the psi f g = True.
findAllSpolynomialsConditional :: (Fractional r, MOrdering o) => 
                                  (Polynomial r o -> Polynomial r o -> Bool) -> 
                                  [Polynomial r o] -> [Polynomial r o] -> [Polynomial r o]
findAllSpolynomialsConditional psi olds news = nub . filter (not . isZero) $ do 
                                  p <- olds ++ news
                                  q <- news
                                  guard $ psi p q
                                  return $ findSpolynomialConditional psi p q

findAllSpolynomials :: (Fractional r, MOrdering o) => 
                                  [Polynomial r o] -> [Polynomial r o] -> [Polynomial r o]
findAllSpolynomials = findAllSpolynomialsConditional (const (const True))

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


-- | Data type for the state of a buchberger computation
data Buchberger r o i =
    Buchberger {
                -- | The GB elements so far
                irrPols :: [Polynomial r o],
                -- | The latest additions to the GB
                newPols :: [Polynomial r o],
                -- | S-polynomials yet to be reduced, sorted by fine grading
                todo    :: M.Map i [Polynomial r o],
                -- | A fine grading of the polynomials. e.g. bigrading
                grading :: Polynomial r o -> i,
                -- | The minimal degrees in a list of degrees
                minElts :: [i] -> [i],
                -- | Selection function for S-polynomials to consider, for partial computations
                pCondition :: (Polynomial r o -> Bool)
               }


-- | Reduce the S-polynomials in the minimal degrees and update GB
buchbergerRedStep :: (Fractional r, MOrdering o, Ord i, Show i) =>
                     Buchberger r o i -> Buchberger r o i

buchbergerRedStep state = 
    if (M.null (todo state)) && (null (newPols state))
    then trace "Done computing." state
    else -- {- 
        trace 
             ("\tReduced degrees: " ++ show minDegs ++ 
              "\n\t\tTotal number of reduction candidates: " ++ 
              show (length . concat . M.elems $ now) ++
              "\n\t\tTotal number surviving reduction: " ++
              show (length newIrrs)) $ -- -}
             buchbergerGenStep $
         state {irrPols = irrPols state ++ newIrrs,
                newPols = newIrrs,
                todo    = wait}
        where minDegs = minElts state $ M.keys $ todo state
              (now, wait) = M.partitionWithKey (\k _ -> k `elem` minDegs) $
                            todo state
              newIrrs = concat $ 
                        fromP $
                        DPH.mapP (flip reduceAllFull (irrPols state)) $
                        toP $
                        M.elems now

-- | Generate the S-polynomials that appear from the updated GB
buchbergerGenStep :: (Fractional r, MOrdering o, Ord i, Show i) =>
                     Buchberger r o i -> Buchberger r o i

buchbergerGenStep state =
    let spols = trace "\tGenerated new S-Polynomials" $
                filter (pCondition state) $
                findAllSpolynomials (irrPols state) (newPols state)
        spolsWithDeg = trace ("\t\tTotal number of new polys: " ++ show (length spols))
                       map (\p -> (grading state p, [p])) spols
        todo' = M.fromListWith (++) spolsWithDeg
        in buchbergerRedStep
           state {todo = M.unionWith (++) (todo state) todo'}

grobnerBasisConditional :: (Fractional r, MOrdering o, Ord i, Show i) =>
                           (Polynomial r o -> i) -> ([i] -> [i]) ->
                           (Polynomial r o -> Bool) ->
                           [Polynomial r o] -> [Polynomial r o]
grobnerBasisConditional degFun minFun pCondFun pols = 
    irrPols $ buchbergerGenStep (Buchberger {irrPols = pols,
                                             newPols = pols,
                                             todo    = M.empty,
                                             grading = degFun,
                                             minElts = minFun,
                                             pCondition = pCondFun})

grobnerBasis :: (Fractional r, MOrdering o, Ord i, Show i) =>
                (Polynomial r o -> i) -> ([i] -> [i]) ->
                [Polynomial r o] -> [Polynomial r o]
grobnerBasis degFun minFun pols = 
    grobnerBasisConditional degFun minFun (const True) pols
