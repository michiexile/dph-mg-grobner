#!/usr/bin/env sage -python

from sage.all import *

def start(matrixWidth):
    R = PolynomialRing(QQ, ['x%s%s' % (m,n) for n in range(matrixWidth) for m in range(matrixWidth)] + ['y%s%s' % (m,n) for n in range(matrixWidth) for m in range(matrixWidth)])
    M = Matrix(R, matrixWidth, R.gens()[0:matrixWidth**2])
    N = Matrix(R, matrixWidth, R.gens()[matrixWidth**2:])
    gens = [f for r in M*N for f in r]
    return (R, gens)
