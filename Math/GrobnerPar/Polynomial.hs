-- Polynomial implementation for the GröbnerPar project
-- © 2010 Jason Dusek, Emil Sköldberg, Mikael Vejdemo-Johansson

module Math.GrobnerPar.Polynomial 

where

import Data.Map as DM
import Math.GrobnerPar.Monomial

data Polynomial r = P (DM.Map Monomial r)