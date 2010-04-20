#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict

class grobner:
    def __init__(self, gens):
        self.comm = MPI.COMM_WORLD
        self.gens = gens
        self.stable = defaultdict(dict)
        self.new = defaultdict(dict)

    def node(self):
        print "I'm a node! I'm number %d!" % self.comm.Get_rank()

    def control(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        for g in self.gens:
            if g.lm() not in self.new[multidegree(g)].keys():
                self.new[multidegree(g)][g.lm()]=[]
            self.new[multidegree(g)][g.lm()].append(g)
        print self.new

