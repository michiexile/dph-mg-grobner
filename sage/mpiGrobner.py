#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict
from mpiGpsql import *

class grobner:
    def __init__(self, gens):
        self.comm = MPI.COMM_WORLD
        self.gens = gens
        self.stable = defaultdict(dict)
        self.new = defaultdict(dict)
        self.sql = None

    def node(self):
        print "I'm a node! I'm number %d!" % self.comm.Get_rank()
        self.comm.Barrier()
        self.sql=sql()
        gens11 = self.sql.loadNew((1,1))
        print gens11 
        for f in gens11:
            if f.lm() not in self.new[multidegree(f)].keys():
                self.new[multidegree(f)][f.lm()]=[]
            self.new[multidegree(f)][f.lm()].append(f)
        print "Node accessed the new things"
        print self.new

    def control(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        self.sql=sql()
        self.comm.Barrier()
        self.sql.storeNew(self.gens)
        for g in self.gens:
            if g.lm() not in self.new[multidegree(g)].keys():
                self.new[multidegree(g)][g.lm()]=[]
            self.new[multidegree(g)][g.lm()].append(g)
        print self.new

