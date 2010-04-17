import Math.GrobnerPar
import Math.GrobnerPar.Polynomial
import Math.GrobnerPar.Monomial
import Math.GrobnerPar.Buchberger

[a,b,c] = nVars 3 :: [Polynomial Rational DegRevLex]

g1 = b^2-a*c
g2 = a^3-c^2

gens = [g1,g2]

main = do
       putStrLn $ unlines $ map show $ grobnerBasis (const 0) id gens
