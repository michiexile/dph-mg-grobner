-- | Cyclic n-roots example
--

import Math.GrobnerPar
import Math.GrobnerPar.Buchberger
import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial

import System.Environment

ej n j | j < 0 = replicate n 0
       | j > n = replicate n 0
       | otherwise = replicate (j-1) 0 ++ [1] ++ replicate (n-j) 0

nVars n = map (monomialTerm . om . M . ej n) [1..n]

cyclicNroots :: Int -> [Polynomial Rational Lex]
cyclicNroots n = product (nVars n) - 1 : lowerTerms
  where
    lowerTerms = [buildLowerTerm k | k <- [0..n-2]]
    buildLowerTerm k = sum . map (product . listVars k) $ [1..n]
    listVars k j = map (((nVars n)!!) . (`mod` n)) [j..j+k]


main = do
  argv <- getArgs
  putStrLn . unlines . map show $ grobnerBasis (cyclicNroots (read (head argv)))
