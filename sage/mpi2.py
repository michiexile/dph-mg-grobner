#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
import sys
import time

import mpiGrobner
import commutingMatrices


def main():
    if len(sys.argv) < 2:
        size=3
    else:
        size=int(sys.argv[1])
    (R, gens) = commutingMatrices.start(size)
    comm = MPI.COMM_WORLD
    myid = comm.Get_rank()
    g = mpiGrobner.grobner(gens)
    tstart = time.time()
    if myid == 0:
        g.control()
    else:
        g.node()
    tstop = time.time()
    print "Process %d executed in %02f s" %(myid,tstop-tstart)


if __name__ == "__main__":
    main()
