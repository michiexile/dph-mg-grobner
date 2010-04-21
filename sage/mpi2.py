#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
import mpiGrobner
import commutingMatrices

(R, gens) = commutingMatrices.start(2)

def main():
    comm = MPI.COMM_WORLD
    myid = comm.Get_rank()
    g = mpiGrobner.grobner(gens)
    if myid == 0:
        g.control()
    else:
        g.node()


if __name__ == "__main__":
    main()
