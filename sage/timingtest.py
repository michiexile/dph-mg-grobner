#!/usr/bin/env sage -python

from sage.all import *
import sys
import time

import serialGrobner
import commutingMatrices


def main():
    if len(sys.argv) < 2:
        size=3
    else:
        size=int(sys.argv[1])
    (R, gens) = commutingMatrices.start(size)
    g = serialGrobner.grobner(gens)
    tstart = time.time()
    g.run()
    print "Grobner basis length: %d" % len(g.gb)
    tstop = time.time()
    print "Process executed in %02f s" % (tstop-tstart)


if __name__ == "__main__":
    main()
