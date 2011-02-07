#!/usr/bin/env sage -python

from sage.all import *
import scipy
import mpiGpsql
import time

vars = "abcdefghijklmnopqrstuvwxyz"

def start(dim,deg):
    R = PolynomialRing(QQ, ['%s%s' % (vars[i],n) for i in range(dim) for n in range(deg)])
    gd = R.gens_dict()
    gs = [['%s%s' % (vars[i],n) for n in range(deg)] for i in range(dim)]
    np = dim+deg+scipy.random.randint(3*(dim+deg))
    gens = []
    for i in range(np):
        degs = [scipy.random.randint(3*deg) for j in range(dim)]
        nmon = scipy.random.randint(5*(dim+deg))
        poly = 0
        for nm in range(nmon):
            coeff = scipy.random.randint(20)
            mon = coeff
            for g in range(dim):
                for k in range(degs[g]):
                    j = scipy.random.randint(deg)
                    mon *= gd[gs[g][j]]
            poly += mon
        gens.append(poly)
    return (R,gens)


    

def timeGB(R,gens):
    I = gens*R
    now = time.time()
    B = I.groebner_basis()
    print "%f seconds" % (time.time()-now)
    print B

def main():
    if len(sys.argv) < 2:
        dim = 3
        deg = 3
    elif len(sys.argv) < 3:
        dim = int(sys.argv[1])
        deg = 3
    else:
        dim = int(sys.argv[1])
        deg = int(sys.argv[2])
    (R,gens) = start(dim,deg)
    timeGB(R,gens)
    sql = mpiGpsql.sql()
    sql.storeNew(gens)


if __name__ == "__main__":
    main()

