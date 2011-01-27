#!/usr/bin/env sage -python

from sage.all import *
from mpi4py import MPI
from mgGrobner import *
from collections import defaultdict
from mpiGpsql import *
from time import strftime, time

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
        self.debugHeader = "Uninitialized:\t\t"
        self.degree = None
        self.running = None
        self.spolyQueue = None
        self.waitingQ = None
        self.assigned = None
        self.running = None
        self.lastsleep = None
        self.synctime = 0
        self.sqltime = 0
        self.spolytime = 0
        self.redtime = 0
    
    def debug(self,dbgstr):
        print dbgTime(), self.debugHeader, dbgstr
    
    def node(self):
        self.nodeSetup()
        while self.running:
            self.status = MPI.Status()
            self.nodeStartloop()
            self.nodeChooseAction()
    
    def nodeSetup(self):
        print "I'm a node! I'm number %d!" % self.comm.Get_rank()
        self.comm.Barrier() # Wait for Control to load SQL first.
        self.sql=sql()
        self.debugHeader = "Node %d:\t\t" % self.comm.Get_rank()
        self.running = True
    
    def nodeStartloop(self):
        self.debug("Sending REQUEST to 0")
        self.comm.send(None,dest=0,tag=REQUEST_NEW_DEGREE)
        self.degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=self.status)
        self.debug("Received from 0: tag: %s" % codeLookup[self.status.Get_tag()])
    
    def nodeSync(self):
        self.debug("Waiting to sync...")
        self.lastsleep = time()
        self.degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=self.status)
        self.synctime += time() - self.lastsleep
        self.debug("Received from 0: tag: %s" % codeLookup[self.status.Get_tag()])
        self.nodeChooseAction()
    
    def nodeChooseAction(self):
        if self.status.Get_tag() == SYNC:
            self.nodeSync()
        elif self.status.Get_tag() == FINISH:
            self.debug("Finishing...")
            self.debug("Total time sync'd:\t%s seconds" % self.synctime)
            self.debug("Total time working SQL:\t%s seconds" % self.sqltime)
            self.debug("Total time reducing:\t%s seconds" % self.redtime)
            self.running = False
        elif self.status.Get_tag() == NEW_DEGREE:
            self.nodeWork(self.degree)
        
    def nodeWork(self, degree):
        self.lastsleep = time()
        candidates = self.sql.loadNew(degree)
        self.sql.dropNew(candidates)
        gb = self.sql.loadStableBelow(degree)
        self.sqltime += time() - self.lastsleep

        reducetime = time()
        doneList = []

        for c in candidates:
            cc = c.reduce(gb)
            if cc == 0:
                continue
                
            gb.append(cc)
            doneList.append(cc)

        self.redtime += time() - reducetime
        del(candidates)
        del(gb)

        self.lastsleep = time()

        gb = self.sql.loadStableAll()

        self.sqltime += time()-self.lastsleep
        self.lastsleep = time()

        newS = filter(lambda p: p!= 0, generateSPolys(gb+doneList,doneList))

        self.redtime += time() - self.lastsleep
        self.lastsleep = time()

        self.sql.storeNew(newS)
        lms = self.sql.storeStable(doneList)

        self.sqltime += time() - self.lastsleep
        
        self.comm.send(lms, dest=0, tag=NEW_GB_DEPOSITED)
    
    def controlSetup(self):
        print "I'm control. We'll deal with: %s" % repr(self.gens)
        self.sql=sql()
        self.comm.Barrier() # Tell Nodes that SQL is loaded.
        
        self.debugHeader = "Node 0:\t\t"
        
        self.sql.storeNew(self.gens)
        
        self.spolyQueue = []
        self.waitingQ = set()
        self.assigned = {}
        self.running = True

    def controlHaveDegree(self):
        self.alldegs = list(IntegerListsLex(self.degree,length=self.degwidth))
        self.innerloop = True

        self.debug("Working with degrees\n\t%s" % repr(self.alldegs))
        
        while self.innerloop:
            # Either we have all processes waiting for us, and no more degrees
            if len(self.waitingQ) == self.comm.Get_size()-1 and self.alldegs == []:
                self.debug("Exhausted.\n\tWaiting: %s\n\tDegrees: %s" % (repr(self.waitingQ),repr(self.alldegs)))
                break
            # Or we have a waiting process and a degree
            if len(self.waitingQ) > 0 and self.alldegs != []:
                self.debug("Sending next degree.\n\tWaiting: %s\n\tDegrees: %s" % (repr(self.waitingQ),repr(self.alldegs)))
                self.controlSendDegreeFromQ()
                continue
            # Or we have either no waiting processes, or no degrees, thus need
            # feedback from our slaves before we can do anything else.
            self.controlReceive()

    def controlSendDegreeFromQ(self):
        deg = self.alldegs.pop()
        if repr(deg) in self.assigned:
            self.debug("Already assigned degree %s:\n\t%s" % (repr(deg),repr(self.assigned)))
            return
        self.alldegs.append(deg)
        dest = self.waitingQ.pop()
        self.controlSendDegree(dest)

    def controlSendDegree(self, dest):
        deg = self.alldegs.pop()
        while repr(deg) in self.assigned and len(self.alldegs) > 0:
            deg = self.alldegs.pop()
        if repr(deg) in self.assigned:
            self.controlSendSync()
            return
        self.debug("Sending to %d new degree %s" % (dest, repr(deg)))
        self.comm.send(deg, dest=dest, tag=NEW_DEGREE)
        self.assigned[repr(deg)] = repr(dest)

    def controlSendSync(self,dest):
        self.comm.send(None, dest=dest, tag=SYNC)
        self.waitingQ.add(dest)

    def controlGenerateSPoly(self,source,data):
        self.lastsleep = time()
        newPolys = self.sql.loadStableByLM(data)
        totalPolys = self.sql.loadStableAll()
        newSP = [p for p in generateSPolys(totalPolys, newPolys)]
        self.sql.storeNew(filter(lambda p: p!=0, newSP))
        self.controlUnassign(self,repr(source))
        self.spolytime += time()-self.lastsleep

    def controlUnassign(self,source):
        items=filter(lambda (v,k): k==source, self.assigned.items())
        for (v,k) in items:
            del(self.assigned[v])

    def controlReceive(self):
        self.status = MPI.Status()
        data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=self.status)
        (tag, source) = (self.status.Get_tag(), self.status.Get_source())
        self.debug("Received from %d tag %s" % (source, codeLookup[tag]))
        self.controlUnassign(repr(source))
        if tag == NEW_GB_DEPOSITED: # We can generate new tasks!
            pass
        else: # Process ready for more. Add to waitingQ. 
            self.comm.send(None, dest=source, tag=SYNC)
            self.debug("Sent to %d sync" % source)
            self.waitingQ.add(source)
            # Cause the Node to block, waiting for a NEW_DEGREE or a FINISH
            # message later on. 


    def controlExhausted(self):
        self.debug("Degrees exhausted.")
        if len(self.waitingQ) == self.comm.Get_size() - 1: # Everyone's waiting
            self.debug("FINISH IT!")
            for dest in self.waitingQ:
                self.comm.send(None, dest=dest, tag=FINISH)
                self.debug("Sent to %d finish" % dest)
            gb = self.sql.loadStableAll()
            print gb
            self.debug("Total S-polynomial time:\t%s" % self.spolytime)
            self.running = False
            return
        self.controlReceive()
        
        

    def control(self):
        self.controlSetup()
        while self.running:
            self.degree = self.sql.findMinimal()

            self.debug("Now treating total degree %s" % repr(self.degree))
            if self.degree != None:
                self.controlHaveDegree()
            else:
                self.controlExhausted()
