#!/usr/bin/env sage -python

from sage.all import *

def sPoly(f, g):
    """Computes the S-polynomial of two polynomials"""
    lf = f.lm()
    lg = g.lm()
    fg = lf.gcd(lg)
    xx = lg.quo_rem(fg)
    xf = f * xx[0]
    return xf.reduce([g])
    

def generateSPolys(olds, news):
    for f in news:
        for g in olds + news:
            yield sPoly(f,g)

def bidegree(mon):
    exp = mon.exponents()[0]
    lexp = len(exp)
    nx = int(lexp/2)
    ny = lexp-nx
    xexp = exp[0:nx]
    yexp = exp[nx:]
    return (sum(xexp),sum(yexp))

multidegree=bidegree

def degreeIdeal(deg,minDeg=0):
    ret = []
    for t in range(minDeg, sum(deg)):
        ret.extend(list(IntegerListsLex(t,length=len(deg),ceiling=list(deg))))
    return ret

