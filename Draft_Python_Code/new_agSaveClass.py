from dataclasses import dataclass, field
import numpy as np
import math
from new_agCon import agcon
from new_agBkg import agBkg

@dataclass
class saveData:
    '''
    Save current data results for plotting.
    The current array of results, xv, is sent in along with current time t.
    '''

    def initiateResults(self, nvar):
        self.answ = np.zeros([4,nvar])
        self.dndep = np.zeros([3,nvar])
        self.dsdep = np.zeros([nvar])
        self.dnflx = np.zeros([3,nvar])
        self.xndep = np.zeros([3, nvar])
        self.nvar = nvar

    def checkTime(self, xv, tcurrent, edov, diam, isw):
        if tcurrent >= 0:
            self.saveYZspreadDervs(xv, edov, diam, isw)

    def saveYZspreadDervs(self, xv, edov, diam, isw):

        for n in range(self.nvar):
            if isw[n] != 0:
                self.answ[0, n] = xv[1, n]
                self.answ[1, n] = xv[2, n]
                self.answ[2, n] = xv[6, n]
                self.answ[3, n] = (edov[n] / diam) ** 3
                self.dndep[0,n] = xv[4,n]
                self.dndep[1,n] = xv[5,n]
                self.dndep[2,n] = 2*xv[7,n]
                tem1 = self.dndep[0,n]
                tem2 = self.dndep[1,n]
                self.dnflx[0,n] = tem2
                self.dnflx[1,n] = -tem1
                self.dnflx[2,n] = self.dndep[2,n]
