#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
import sys
import time

import mpiGrobner

def main():
    comm = MPI.COMM_WORLD
    myid = comm.Get_rank()
    g = mpiGrobner.grobner()
    tstart = time.time()
    if myid == 0:
        g.control()
    else:
        g.node()
    tstop = time.time()
    print "Process %d executed in %02f s" %(myid,tstop-tstart)


if __name__ == "__main__":
    main()
