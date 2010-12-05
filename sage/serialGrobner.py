#!/usr/bin/env sage -python

from sage.all import *
from collections import defaultdict
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
        self.gens = gens
	self.gb = []

    def run(self):
        new = self.gens
        old = []
        mid = []
	    while not new.empty():
		    # generate S-polynomials
		    candidates = [ p for p in generateSPolys(old + mid,new) ]
		    mindeg = min([p.degree() for p in candidates])

		    # reduce S-polynomials
		    rnew = []
		    for c in candidates:
			    cc = c.reduce(gb)
			    if cc == 0:
				    continue
			    rnew.append(cc)

            # update lists
            old = old + [p for p in mid if deg(p) < mindeg]
            mid += new
            mid = [p for p in mid if deg(p) >= mindeg]
            mid = [p for p in [q.reduce(rnew) for q in mid] if p != 0]
			new = rnew
	self.gb = old + mid
