#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict
from mpiGpsql import *
from time import strftime

REQUEST_NEW_DEGREE = 0
NEW_DEGREE = 1
NEW_GB_DEPOSITED = 2
SYNC = 3
FINISH = 4

codeLookup = {
    0: "REQUEST_NEW_DEGREE",
    1: "NEW_DEGREE",
    2: "NEW_GB_DEPOSITED",
    3: "SYNC",
    4: "FINISH",
}

def dbgTime():
	return strftime("%H:%M:%S")

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
        debugHeader = "Node %d:\t\t" % self.comm.Get_rank()
        while True:
            status = MPI.Status()
            print dbgTime(), debugHeader, "Sending REQUEST to 0"
            self.comm.send(None,dest=0,tag=REQUEST_NEW_DEGREE)
            degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
            print dbgTime(), debugHeader, "Received from 0: tag: %s" % codeLookup[status.Get_tag()]
            if status.Get_tag() == SYNC:
                print dbgTime(), debugHeader, "Waiting to sync..."
                degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
                print dbgTime(), debugHeader, "Received from 0: tag: %s" % codeLookup[status.Get_tag()]
            if status.Get_tag() == FINISH:
                print dbgTime(), debugHeader, "Finishing..."
                return
            if status.Get_tag() == NEW_DEGREE:
                self.nodeWork(degree)

    def nodeWork(self, degree):
        candidates = self.sql.loadNew(degree)
        gb = self.sql.loadStableBelow(degree)
        
        doneList = []

        for c in candidates:
            cc = c.reduce(gb)
            if cc == 0: 
                continue

            gb.append(cc)
            doneList.append(cc)

        lms = self.sql.storeStable(doneList)
        self.sql.dropNew([degree])

        self.comm.send(lms, dest=0, tag=NEW_GB_DEPOSITED)
            

    def control(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        self.sql=sql()
        self.comm.Barrier() # Tell Nodes that SQL is loaded.

        debugHeader = "Node 0:\t\t"

        self.sql.storeNew(self.gens)
        
        spolyQueue = []
        waitingQ = set()
        assigned = {}
        while True:
            degree = self.sql.findMinimal()

            print dbgTime(), debugHeader, "Now treating total degree %s" % repr(degree)
            if degree != None:
                alldegs = list(IntegerListsLex(degree,length=self.degwidth))

                while True:
                    if len(waitingQ) == self.comm.Get_size()-1 and alldegs == []:
                        # All are waiting for more to do. Next degree!
                        break

                    if len(waitingQ) > 0 and alldegs != []:
                        deg = alldegs.pop()
                        if repr(deg) in assigned:
                            continue
                        dest = waitingQ.pop()
                        print dbgTime(), debugHeader, "Sending to %d new degree %s" % (dest,repr(deg))
                        self.comm.send(deg, dest=dest, tag=NEW_DEGREE)
                        assigned[repr(deg)]=repr(dest)
                        continue

                    status = MPI.Status()
                    data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
                    (tag, source) = (status.Get_tag(), status.Get_source())
                    print dbgTime(), debugHeader, "Received from %d tag %s" % (source, codeLookup[tag])
                    if tag == REQUEST_NEW_DEGREE:
                        if alldegs == []:
                            self.comm.send(None, dest=dest, tag=SYNC)
                            waitingQ.add(source)
                            continue

                        deg = alldegs.pop()
                        if repr(deg) in assigned:
                            self.comm.send(None,dest=source,tag=SYNC)
                            waitingQ.add(source)
                            continue

                        dest = source
                        print dbgTime(), debugHeader, "Sending to %d new degree %s" % (dest,repr(deg))
                        self.comm.send(deg, dest=dest,tag=NEW_DEGREE)
                        assigned[repr(deg)]=dest
                    elif tag == NEW_GB_DEPOSITED:
                        newPolys = self.sql.loadStableByLM(map(eval,data))
                        totalPolys = self.sql.loadStableAll()
                        newSP = [p for p in generateSPolys(totalPolys, newPolys)]
                        self.sql.storeNew(filter(lambda p: p!=0, newSP))
                        items=filter(lambda (v,k): v==source, assigned.items())
                        for (v,k) in items:
                            del(assigned[k])
            else:
                print dbgTime(), debugHeader, "Degrees exhausted"
                if len(waitingQ) == self.comm.Get_size() - 1:
                    print dbgTime(), debugHeader, "FINISH IT!"
                    for dest in waitingQ:
                        self.comm.send(None, dest=dest, tag=FINISH)
                        print dbgTime(), debugHeader, "Sent to %d finish" % dest
                    gb = self.sql.loadStableAll()
                    print gb
                    return
                    
                status = MPI.Status()
                data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
                (tag, source) = (status.Get_tag(), status.Get_source())
                print dbgTime(), debugHeader, "Received from %d tag %s" % (source, codeLookup[tag])
                if tag == NEW_GB_DEPOSITED:
                    newPolys = self.sql.loadStableByLM(data)
                    oldPolys = self.sql.loadStableAll()
                    newSP = [p for p in generateSPolys(oldPolys, newPolys)]
                    self.sql.storeNew(newSP)
                    items=filter(lambda (v,k): v==source, assigned.items())
                    for (v,k) in items:
                        del(assigned[k])
                else:
                    self.comm.send(None, dest=source, tag=SYNC)
                    print dbgTime(), debugHeader, "Sent to %d sync" % source
                    waitingQ.add(source)
                    # Cause the Node to block, waiting for a NEW_DEGREE or a FINISH
                    # message later on. 
