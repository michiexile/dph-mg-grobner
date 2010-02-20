-- | Commuting matrices example
--

import Math.GrobnerPar
import Math.GrobnerPar.Buchberger
import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial

import Data.Array

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

ej n j | j < 0 = replicate n 0
       | j > n = replicate n 0
       | otherwise = replicate (j-1) 0 ++ [1] ++ replicate (n-j) 0

nVars n = map (monomialTerm . om . M . ej n) [1..n]


main = do
  argv <- getArgs
  putStrLn . unlines . map show $
           grobnerBasis (commutingMatrices (read (head argv)))
