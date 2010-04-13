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

commutingMatrices :: Int -> [Polynomial Rational DegRevLex]
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


biDegreeOM :: (MOrdering o) => OrderedMonomial o -> (Int, Int)
biDegreeOM m = (sum . take k $ ex, sum . drop k $ ex) 
             where
               ex = exponents . monomial $ m
               k = (length ex) `div` 2

biDegree :: (MOrdering o, Num r) => Polynomial r o -> (Int, Int)
biDegree = biDegreeOM . fst . leadingTerm

-- | Given a list of bidegrees, return the minimal ones wrt divisibility
minBiDegs :: [(Int, Int)] -> [(Int, Int)]
minBiDegs xs = filter (\ (i, j) -> i + j == minDeg xs) xs
    where minDeg xs = minimum $ map (\ (i, j) -> i + j) xs

totalDegreeFilter :: (MOrdering o, Num r) =>
                     Int -> (Polynomial r o) -> Bool

totalDegreeFilter n p = (<= n) . totalDegree $ ltP
           where
             ltP = (fst . leadingTerm $ p) 

main = do
  argv <- getArgs
  let
         mSize = read (head argv)
         spCond | length argv == 1 = (const True)
                | otherwise = totalDegreeFilter maxD
             where
               maxD = read (argv!!1)
  putStrLn . unlines . map show $
           grobnerBasisConditional biDegree minBiDegs spCond
                            (commutingMatrices mSize)
