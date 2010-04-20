#!/usr/bin/env sage -python

from mgGrobner import *
from sqlalchemy import *

class sql:
    def __init__(self):
        self.engine = create_engine("sqlite:////tmp/mpigrobner.db", echo=False)
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
        inserts = map(lambda f: {'degree': str(multidegree(f)), 'poly': f},fs)
        self.stable.insert().execute(inserts)

    def storeNew(self, fs):
        inserts = map(lambda f: {'degree': str(multidegree(f)), 'poly': f},fs)
        self.new.insert().execute(inserts)


    def loadStable(self,deg):
        select = self.stable.select()
        select.where(self.stable.c.degree == str(deg))
        selects = select.execute()
        ret = [s[self.stable.c.poly] for s in selects]
        selects.close()
        return ret

    def loadStableBelow(self,deg):
        ret = []
        for d in degreeIdeal(deg):
            if d != deg:
                ret.extend(loadStable(d))
        return ret

    def loadNew(self,deg):
        select = self.new.select([])
        select.where(self.new.c.degree == str(deg))
        selects = select.execute()
        ret = [s[self.new.c.poly] for s in selects]
        selects.close()
        return ret

