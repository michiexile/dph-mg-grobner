#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict
from mpiGpsql import *

REQUEST_NEW_DEGREE = 0
NEW_DEGREE = 1
NEW_GB_DEPOSITED = 2
FINISH = 3

class grobner:
    def __init__(self, gens):
        self.comm = MPI.COMM_WORLD
        self.gens = gens
        self.stable = defaultdict(dict)
        self.new = defaultdict(dict)
        self.sql = None
        self.degwidth = len(multidegree(gens[0]))

    def node(self):
        print "I'm a node! I'm number %d!" % self.comm.Get_rank()
        self.comm.Barrier() # Wait for Control to load SQL first.
        self.sql=sql()

        while True:
            self.comm.send(None,dest=0,tag=REQUEST_NEW_DEGREE)
            degree = self.comm.recv(source=0,tag=NEW_DEGREE)
            degree = tuple(degree)

            candidates = self.sql.loadNew(degree)
            gb = self.sql.loadStableBelow(degree)

            print "Computation on degree %s" % repr(degree)
            doneList = []

            for c in candidates:
                cc = c.reduce(gb)
                if cc != 0:
                    gb.append(cc)
                    doneList.append(cc)
                    self.sql.storeStable([cc])

            self.sql.dropNew(candidates)
            
            print doneList

            self.comm.send(doneList, dest=0, tag=NEW_GB_DEPOSITED)


    def control(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        self.sql=sql()
        self.comm.Barrier() # Tell Nodes that SQL is loaded.

        self.sql.storeNew(self.gens)
        
        spolyQueue = []
        while True:
            degree = self.sql.findMinimal()
            print "Now treating total degree %d" % degree
            alldegs = list(IntegerListsLex(degree,length=self.degwidth))

            while True:
                status = MPI.Status()
                data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
                (tag, source) = (status.Get_tag(), status.Get_source())
                if tag == REQUEST_NEW_DEGREE:
                    self.comm.send(alldegs.pop(), dest=source,tag=NEW_DEGREE)
                    if alldegs==[]:
                        break
                if tag == NEW_GB_DEPOSITED:
                    print "Queueing up for new S-polynomial generation"
