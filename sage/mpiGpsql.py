#!/usr/bin/env sage -python

from mgGrobner import *
from sqlalchemy import *
from sage.all import *

class sql:
    def __init__(self):
        self.engine = create_engine("sqlite:////tmp/mpigrobner.db", echo=False)
        self.metadata = MetaData()
        self.stable = Table('stable', self.metadata, 
                       Column('leadingmonomial', String(100), primary_key=True),
                       Column('degree', String(100), nullable=False), 
                       Column('poly', PickleType(),nullable=False))
        self.new = Table('new', self.metadata, 
                    Column('leadingmonomial', String(100)),
                    Column('degree', String(100),nullable=False), 
                    Column('poly', PickleType(),nullable=False))
        self.metadata.create_all(self.engine)
        self.metadata.bind = self.engine

    def storeStable(self,fs):
        if fs == []: return []
        inserts = map(lambda f: {'leadingmonomial': repr(f.lm().exponents()), 'degree': repr(multidegree(f)), 'poly': f},fs)
        ins = self.stable.insert()
        res = ins.execute(inserts)
        return map(lambda f: repr(f.lm().exponents()), fs)

    def storeNew(self, fs):
        if fs == []: return []
        inserts = map(lambda f: {'leadingmonomial': repr(f.lm().exponents()), 'degree': repr(multidegree(f)), 'poly': f},fs)
        ins = self.new.insert()
        res = ins.execute(inserts)
        return map(lambda f: repr(f.lm().exponents()), fs)

    def loadStable(self,deg):
        sel = select([self.stable.c.poly],self.stable.c.degree == repr(deg))
        selects = sel.execute()
        ret = [s[self.stable.c.poly] for s in selects]
        selects.close()
        return ret

    def loadStableBelow(self,deg):
        ret = []
        for d in degreeIdeal(deg):
            ret.extend(self.loadStable(d))
        return ret

    def loadStableByLM(self, lmexs):
        ret = []
        for lm in lmexs:
            sel = select([self.stable.c.poly], self.stable.c.leadingmonomial == repr(lm))
            selects = sel.execute()
            ret.extend([s[self.stable.c.poly] for s in selects])
            selects.close()
        return ret

    def loadStableAll(self):
        sel = select([self.stable.c.poly])
        selects = sel.execute()
        ret = [s[self.stable.c.poly] for s in selects]
        return ret

    def loadNew(self,deg):
        sel = select([self.new.c.poly],self.new.c.degree == repr(deg))
        selects = sel.execute()
        ret = [s[self.new.c.poly] for s in selects]
        selects.close()
        return ret

    def dropStable(self, lmexs):
        for f in lmexs:
            delQ = self.stable.delete(self.stable.c.leadingmonomial == f)
            delQ.execute()

    def dropNew(self, degs):
        for d in degs:
            delQ = self.new.delete(self.new.c.degree == repr(d))
            delQ.execute()

    def findMinimal(self):
        sel = select([self.new.c.degree],distinct=True)
        degs = sel.execute()
        totdegs = [sum(eval(d[self.new.c.degree])) for d in degs]
        degs.close()
        if totdegs == []:
            mindeg = None
        else:
            mindeg = min(totdegs)
        return mindeg