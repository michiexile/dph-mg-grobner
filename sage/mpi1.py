#!/usr/bin/env sage

from mpi4py import MPI 

comm = MPI.COMM_WORLD
myid = comm.Get_rank()
numprocs = comm.Get_size()

print "Hello from ", myid
print "Numprocs is ", numprocs

