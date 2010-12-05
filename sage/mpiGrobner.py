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
        self.debugHeader = "Uninitialized:\t\t"
        self.degree = None
        self.running = None
        self.spolyQueue = None
        self.waitingQ = None
        self.assigned = None
        self.running = None
    
    def debug(self,dbgstr):
        print dbgTime(), self.debugHeader, dbgstr
    
    def node(self):
        self.nodeSetup()
        while self.running:
            status = MPI.Status()
            self.nodeStartloop()
            self.nodeChooseAction(status)
    
    def nodeSetup(self):
        print "I'm a node! I'm number %d!" % self.comm.Get_rank()
        self.comm.Barrier() # Wait for Control to load SQL first.
        self.sql=sql()
        self.debugHeader = "Node %d:\t\t" % self.comm.Get_rank()
        self.running = True
    
    def nodeStartloop(self):
        self.debug("Sending REQUEST to 0")
        self.comm.send(None,dest=0,tag=REQUEST_NEW_DEGREE)
        self.degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
        self.debug("Received from 0: tag: %s" % codeLookup[status.Get_tag()])
    
    def nodeSync(self):
        self.debug("Waiting to sync...")
        self.degree = self.comm.recv(source=0,tag=MPI.ANY_TAG,status=status)
        self.debug("Received from 0: tag: %s" % codeLookup[status.Get_tag()])
        self.nodeChooseAction(status)
    
    def nodeChooseAction(self, status):
        if status.Get_tag() == SYNC:
            nodeSync()
        elif status.Get_tag() == FINISH:
            self.debug("Finishing...")
            self.running = False
        elif status.Get_tag() == NEW_DEGREE:
            self.nodeWork(self.degree)
        
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
        
        while self.innerloop:
            # Either we have all processes waiting for us, and no more degrees
            if len(waitingQ) == self.comm.Get_size()-1 and self.alldegs == []:
                break
            # Or we have a waiting process and a degree
            if len(waitingQ) > 0 and self.alldegs != []:
                self.controlSendDegreeFromQ()
                continue
            # Or we have either no waiting processes, or no degrees, thus need
            # feedback from our slaves before we can do anything else.
            status = MPI.Status()
            data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
            (tag, source) = (status.Get_tag(), status.Get_source())
            self.debug("Received from %d tag %s" % (source, codeLookup[tag]))
            if tag == REQUEST_NEW_DEGREE:
                if self.alldegs == []: # We have nothing to send
                    self.controlSendSync(source)
                    continue
                self.controlSendDegree(source)
            elif tag == NEW_GB_DEPOSITED:
                self.controlGenerateSPoly()

    def controlSendDegreeFromQ(self):
        deg = self.alldegs.pop()
        if repr(deg) in self.assigned:
            return
        self.alldegs.push(deg)
        dest = self.waitingQ.pop()
        self.sendDegree(dest)

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
        self.waitingQ.add(source)

    def controlGenerateSPoly(self):
        newPolys = self.sql.loadStableByLM(map(eval,data))
        totalPolys = self.sql.loadStableAll()
        newSP = [p for p in generateSPolys(totalPolys, newPolys)]
        self.sql.storeNew(filter(lambda p: p!=0, newSP))
        items=filter(lambda (v,k): v==source, assigned.items())
        for (v,k) in items:
            del(assigned[k])

    def controlExhausted(self):
        self.debug("Degrees exhausted.")
        if len(waitingQ) == self.comm.Get_size() - 1: # Everyone's waiting
            self.debug("FINISH IT!")
            for dest in waitingQ:
                self.comm.send(None, dest=dest, tag=FINISH)
                self.debug("Sent to %d finish" % dest)
            gb = self.sql.loadStableAll()
            print gb
            self.running = False
        status = MPI.Status()
        data = self.comm.recv(source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG, status=status)
        (tag, source) = (status.Get_tag(), status.Get_source())
        self.debug("Received from %d tag %s" % (source, codeLookup[tag]))
        if tag == NEW_GB_DEPOSITED: # We can generate new tasks!
            newPolys = self.sql.loadStableByLM(data)
            oldPolys = self.sql.loadStableAll()
            newSP = [p for p in generateSPolys(oldPolys, newPolys)]
            self.sql.storeNew(newSP)
            items=filter(lambda (v,k): v==source, assigned.items())
            for (v,k) in items:
                del(assigned[k])
        else: # We have nothing more to give. Go wait for news.
            self.comm.send(None, dest=source, tag=SYNC)
            print dbgTime(), debugHeader, "Sent to %d sync" % source
            waitingQ.add(source)
            # Cause the Node to block, waiting for a NEW_DEGREE or a FINISH
            # message later on. 
        
        

    def control(self):
        self.controlSetup()
        while self.running:
            self.degree = self.sql.findMinimal()

            self.debug("Now treating total degree %s" % repr(self.degree))
            if self.degree != None:
                self.controlHaveDegree()
            else:
                self.controlExhausted()
