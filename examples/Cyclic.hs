-- | Cyclic n-roots example
--

import Math.GrobnerPar
import Math.GrobnerPar.Buchberger
import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Polynomial

import System.Environment


cyclicNroots :: Int -> [Polynomial Rational Lex]
cyclicNroots n = product (nVars n) - 1 : lowerTerms
  where
    lowerTerms = [buildLowerTerm k | k <- [0..n-2]]
    buildLowerTerm k = sum . map (product . listVars k) $ [1..n]
    listVars k j = map (((nVars n)!!) . (`mod` n)) [j..j+k]


main = do
  argv <- getArgs
  putStrLn . unlines . map show $
       grobnerBasis (const 0) id (cyclicNroots (read (head argv)))
