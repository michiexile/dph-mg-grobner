#!/usr/bin/env sage -python

from mgGrobner import *
from sqlalchemy import *

class sql:
    def __init__(self):
        self.engine = create_engine("sqlite:////tmp/mpigrobner.db", echo=True)
        self.metadata = MetaData()
        self.stable = Table('stable', self.metadata, 
                       Column('id', Integer, primary_key=True),
                       Column('degree', String(100), nullable=False), 
                       Column('poly', PickleType(),nullable=False))
        self.new = Table('new', self.metadata, 
                    Column('id', Integer, primary_key=True),
                    Column('degree', String(100),nullable=False), 
                    Column('poly', PickleType(),nullable=False))
        self.metadata.create_all(self.engine)
        self.metadata.bind = self.engine

    def storeStable(self,fs):
        inserts = map(lambda f: {'degree': repr(multidegree(f)), 'poly': f},fs)
        self.stable.insert().execute(inserts)

    def storeNew(self, fs):
        inserts = map(lambda f: {'degree': repr(multidegree(f)), 'poly': f},fs)
        self.new.insert().execute(inserts)


    def loadStable(self,deg):
        sel = select([self.stable.c.poly],self.stable.c.degree == repr(deg))
        selects = sel.execute()
        ret = [s[self.stable.c.poly] for s in selects]
        selects.close()
        return ret

    def loadStableBelow(self,deg):
        ret = []
        for d in degreeIdeal(deg):
            if d != deg:
                ret.extend(self.loadStable(d))
        return ret

    def loadNew(self,deg):
        sel = select([self.new.c.poly],self.new.c.degree == repr(deg))
        print sel
        selects = sel.execute()
        ret = [s[self.new.c.poly] for s in selects]
        selects.close()
        return ret

    def dropStable(self, fs):
        for f in fs:
            delQ = self.stable.delete(self.stable.c.poly == f)
            delQ.execute()

    def dropNew(self, fs):
        for f in fs:
            delQ = self.new.delete(self.new.c.poly == f)
            delQ.execute()

    def findMinimal(self):
        sel = select([self.new.c.degree],distinct=True)
        degs = sel.execute()
        totdegs = [sum(eval(d[self.new.c.degree])) for d in degs]
        degs.close()
        return min(totdegs)
