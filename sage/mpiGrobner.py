#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict
from mpiGpsql import *

REQUEST_NEW_DEGREE = 0
NEW_DEGREE = 1
NEW_GB_DEPOSITED = 2
SYNC = 3
FINISH = 4

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
            status = MPI.Status()
            self.comm.send(None,dest=0,tag=REQUEST_NEW_DEGREE)
            degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
            if status.Get_tag() == SYNC:
                print "Waiting to sync..."
                degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
                if status.Get_tag() == FINISH:
                    print "Finishing..."
                    return
            if status.Get_tag() == NEW_DEGREE:
                self.nodeWork(degree)

    def nodeWork(self, degree):
        candidates = self.sql.loadNew(degree)
        gb = self.sql.loadStableBelow(degree)
        
        print "Computation on degree %s" % repr(degree)
        doneList = []

        for c in candidates:
            #print "\tReducing: %s" % repr(c)
            #print "\tUsing: %s" % repr(gb)
            cc = c.reduce(gb)
            #print "\tTo: %s" % repr(cc)
            if cc == 0: 
                continue

            gb.append(cc)
            doneList.append(cc)

        lms = self.sql.storeStable(doneList)
        self.sql.dropNew([degree])

        print "\tNew GB elements: %d" % len(lms)
        
        self.comm.send(lms, dest=0, tag=NEW_GB_DEPOSITED)
            

    def control(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        self.sql=sql()
        self.comm.Barrier() # Tell Nodes that SQL is loaded.

        self.sql.storeNew(self.gens)
        
        spolyQueue = []
        waitingQ = []
        while True:
            degree = self.sql.findMinimal()

            print "Now treating total degree %s" % repr(degree)
            if degree != None:
                alldegs = list(IntegerListsLex(degree,length=self.degwidth))

                if waitingQ != []:
                    for dest in waitingQ:
                        self.comm.send(alldesg.pop(), dest=dest, tag=NEW_DEGREE)

                while True:
                    status = MPI.Status()
                    data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
                    (tag, source) = (status.Get_tag(), status.Get_source())
                    if tag == REQUEST_NEW_DEGREE:
                        self.comm.send(alldegs.pop(), dest=source,tag=NEW_DEGREE)
                        if alldegs==[]:
                            break
                    elif tag == NEW_GB_DEPOSITED:
                        print "Queueing up for new S-polynomial generation"
                        #print "\tReceived new GB list: %s" % repr(data)
                        newPolys = self.sql.loadStableByLM(map(eval,data))
                        print "\tNew GB elements: %d" % len(newPolys)
                        totalPolys = self.sql.loadStableAll()
                        print "\tTotal GB size: %d" % len(totalPolys)
                        newSP = [p for p in generateSPolys(totalPolys, newPolys)]
                        print "\tQueued up: %d" % len(newSP)
                        self.sql.storeNew(filter(lambda p: p!=0, newSP))
            else:
                print "Degrees exhausted"
                status = MPI.Status()
                data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
                (tag, source) = (status.Get_tag(), status.Get_source())
                if tag == NEW_GB_DEPOSITED:
                    print "Queueing up for new S-polynomial generation"
                    newPolys = self.sql.loadStableByLM(data)
                    oldPolys = self.sql.loadStableAll()
                    newSP = [p for p in generateSPolys(oldPolys, newPolys)]
                    print "\tQueued up: %d" % len(newSP)
                    self.sql.storeNew(newSP)
                else:
                    self.comm.send(None, dest=source, tag=SYNC)
                    waitingQ.append(source)
                    print "Added %d to holding pattern queue" % source
                    if len(waitingQ) == self.comm.Get_size()-1:
                        print "FINISH IT!"
                        for dest in waitingQ:
                            self.comm.send(None, dest=dest, tag=FINISH)
                        gb = self.sql.loadStableAll()
                        print gb
                        return
                    # Cause the Node to block, waiting for a NEW_DEGREE or a FINISH
                    # message later on. 
