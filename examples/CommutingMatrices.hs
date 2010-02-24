-- | Commuting matrices example
--

import Math.GrobnerPar
import Math.GrobnerPar.Buchberger
import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial

import Data.Array
import Data.List (nub)
import Debug.Trace

import System.Environment

commutingMatrices :: Int -> [Polynomial Rational Lex]
commutingMatrices n =
    let xyvars = nVars (2*n*n)
        xvars = take (n*n) xyvars
        yvars = drop (n*n) xyvars
        xarr = listArray ((1,1), (n,n)) xvars
        yarr = listArray ((1,1), (n,n)) yvars
        xyarr =  listArray ((1,1), (n,n)) $
                 do i <- [1..n]
                    j <- [1..n]
                    return $ sum $ do k <- [1..n]
                                      return ((xarr!(i,k)) * (yarr!(k,j)))
        yxarr =  listArray ((1,1), (n,n)) $
                 do i <- [1..n]
                    j <- [1..n]
                    return $ sum $ do k <- [1..n]
                                      return ((yarr!(i,k)) * (xarr!(k,j)))
        in zipWith (-) (elems xyarr) (elems yxarr)

biDegreeOM :: (MOrdering o) => 
            OrderedMonomial o -> (Int, Int)
biDegreeOM m = (sum . take k $ ex, sum . drop k $ ex) 
             where
               ex = exponents . monomial $ m
               k = (length ex) `div` 2

biDegree :: (MOrdering o, Num r) =>
            Polynomial r o -> (Int, Int)
biDegree = biDegreeOM . fst . leadingTerm

testFixedBidegree :: (MOrdering o, Num r) =>
                      (Int, Int) -> Polynomial r o -> Polynomial r o -> Bool
testFixedBidegree (x,y) p q = (x,y) == (biDegreeOM lcmPQ) 
                               where
                                 (lP,_) = leadingTerm p
                                 (lQ,_) = leadingTerm q
                                 lcmPQ = lcmOM lP lQ

allBidegrees :: Int -> [(Int, Int)]
allBidegrees n = [(k,n-k) | k <- [0..n]]

grobnerStep :: (MOrdering o, Fractional r) =>
               Int -> [Polynomial r o] -> [Polynomial r o]
grobnerStep d basis = nub . filter (not . isZero) $
                      concatMap (\bd -> trace ("Treating degree: " ++ show bd ++ "\n") $ 
                                        grobnerBasisConditional (testFixedBidegree bd) basis) 
                                (allBidegrees d)

grobnerAcc :: (MOrdering o, Fractional r) =>
           Int -> [Polynomial r o] -> [Polynomial r o]
grobnerAcc curDeg basis | curDeg > maxDeg = basis
                        | otherwise = grobnerAcc (curDeg+1) newBasis
    where
      maxDeg = maximum (map (totalDegree . fst . leadingTerm) basis)
      newBasis = nub . filter (not . isZero) $ basis ++ grobnerStep curDeg basis

main = do
  argv <- getArgs
  putStrLn . unlines . map show $
           grobnerAcc 0 (commutingMatrices (read (head argv)))
