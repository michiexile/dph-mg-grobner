#!/usr/bin/env sage -python

import mgGrobner

class pgsql:
    def __init__(self):
        self.conn = None

    def storeStable(self,fs):
        pass

    def storeNew(self, fs):
        pass

    def loadStable(self,deg):
        return []

    def loadStableBelow(self,deg):
        ret = []
        for d in degreeIdeal(deg):
            if d != deg:
                ret.extend(loadStable(d))
        return ret

    def loadNew(self,deg):
        return []
